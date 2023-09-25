match_string <- function(name,
                        wiki,
                        lname_cut = 2,
                        fuzzy = F) {
  #initialize result list
  matches = wiki[1,] %>%
    select(itemLabel,id) %>%
    mutate(reason = "nonewhatsoever")
  
  #exact full match between parsed name and wikidata item label
  matches = wiki %>%
    filter(itemLabel == name$parsed) %>%
    select(itemLabel,id) %>%
    mutate(reason = "exact_match") %>%
    rbind(matches,.)
  
  #exact match of parsed surname and presumed wikidata item surname from label
  if (!is.na(name$surname)&
             name$surname!="") {
    matches = wiki %>%
      filter(surname == name$surname) %>%
      select(itemLabel,id) %>%
      mutate(reason = "surname_match") %>%
      rbind(matches,.)
  }
  
  #exact match of parsed first name and presumed wikidata item first name from label
  if (!is.na(name$fname)&
             name$fname!="") {
    matches = wiki %>%
      filter(fname == name$fname) %>%
      select(itemLabel,id) %>%
      mutate(reason = "firstname_match") %>%
      rbind(matches,.)
  }
  #fuzzy match of parsed surname into any presumed wikidata item surname from label
  #max distance between both str lengths set by lname_cut
  #default is 2, so surname in wikidata can only be 1 char longer at most
  if (fuzzy&
      !is.na(name$surname)&
             name$surname!="") {
    matches = wiki %>%
      filter(agrepl(name$surname,
                    surname)) %>%
      filter(nchar(surname)-lname_cut < nchar(name$surname)) %>%
      select(itemLabel,id) %>%
      mutate(reason = "surname_fuzzy_match") %>%
      rbind(matches,.)
  }
  #exact match of the interpreted initials
  if (!is.na(name$initials)&
             name$initials!="") {
    matches = wiki %>%
      filter(initials == name$initials) %>%
      select(itemLabel,id) %>%
      mutate(reason = "initials_match") %>%
      rbind(matches,.)
    
    matches = wiki %>%
      filter(outer_initials == name$outer_initials) %>%
      select(itemLabel,id) %>%
      mutate(reason = "initials_outer_match") %>%
      rbind(matches,.)
    
  }
  
  matches = matches[-1,]
  
  return(matches)
}

match_validate <- function(result,
                           rmode = "best",#can also be "cut" or "all"
                           cut,#if cut, then the nr of results to include in the return
                           minscore = 10) {#results below this score will be dropped
  
  #rank each matching method
  #exact matches are best (on alias or english label)
  #then exact matches on first or surname
  #then fuzzy matches on surname
  #then initials
  ranking = tibble(
    reason = c("exact_match",
               "surname_match",
               "firstname_match",
               "surname_fuzzy_match",
               "initials_match",
               "initials_outer_match"),
    value = c(1000,
              100,
              100,
              10,
              2,
              1)
  )
  
  #nomatch if no match was found or only initials matched
  nomatch = tibble(id = as.character(NA),
                   reasons = as.character(NA),
                   labels = as.character(NA),
                   score = 0)
  
  if (dim(result)[1] == 0) {
    return(nomatch[-1,])
  }
  
  #collapse each result in a list of each possible wikidata qid
  #and sum the scores
  result %<>%
    left_join(ranking,by="reason") %>%
    mutate(aliaskey = paste(id,itemLabel,sep=":")) %>%
    group_by(aliaskey) %>%
    summarize(reasons = paste(unique(reason),collapse = "|"),
              itemLabel = first(itemLabel),
              id = first(id),
              score = sum(value)) %>%
    group_by(id) %>%
    summarize(reasons = paste(reasons,collapse = "||"),
              itemLabel = paste(itemLabel,collapse = "|"),
              score = max(score)) %>%
    filter(score >= minscore) %>%
    arrange(desc(score))
  
  if (rmode == "best") {
    return(result[1,])
  } else if (rmode == "cut") {
    if (!is.null(cut)) {
      return(result[1:min(cut,
                          dim(result)[1])])
    } else {
      stop("Cut parameter not specified.")
    }
  } else if (rmode == "all") {
    return(result)
  }
}

assess_cores <- function(cores) {
  if (grepl("+",cores)) {
    require(doParallel)
    cores = max(detectCores(logical = F),
                gsub("\\+",
                     "",
                     cores),
                na.rm = T)
  }
  return(as.numeric(cores))
}

threading <- function(data,#list or tibble to multithread
                      f,#function to apply to data, str or function object
                      num_threads = 4,#nr of threads to use
                      req_args = NULL,#vector with names of objects to load from globalenv
                      pkg = c("tidyverse",#required packages, might be unneeded argument
                              "magrittr"),
                      srcs = "src/matching.R",#required script files, might be unneeded argument
                      dryrun = F) {#for testing
  start_overhead_time = Sys.time()
  require(parallel)
  require(doParallel)
  
  #convert data to list if it's a tibble
  if (is_tibble(data)) {
    data %<>%
      split(seq(dim(data)[1]))
  }
  
  if (!dryrun) {
    #create the cluster for threading
    cl = makePSOCKcluster(num_threads)
    registerDoParallel(cl)
    
    #load the required objects into the cluster
    if (!is.null(req_args)) {
      clusterExport(cl,
                    unlist(req_args))
    }
    
    #load the arguments that specify other requirements in the cluster
    clusterExport(cl,
                  c("pkg","srcs"),
                  envir = environment())
    
    #execute those other requirements
    clusterEvalQ(cl, {
      for (i in 1:length(pkg)) {
        library(pkg[i],
                character.only = T)
      }
      for (i in 1:length(srcs)) {
        source(srcs[i])
      }
    })
    
    #wrap the function to be used to avoid argument misspecification
    #wrapping might no longer be needed after much troubleshooting
    #but don't break things that work
    custom_wrapper <- function(arg) {
      if (!is.null(req_args)) {
        do.call(f,
                c(list(arg),lapply(req_args,get)))
      } else {
        do.call(f,
                list(arg))
      }
    }
    
    #multithread the process, log time
    start_time = Sys.time()
    resu = foreach(mat = data) %dopar% custom_wrapper(mat)
    end_time = Sys.time()
    stopCluster(cl)
    
    parallel_time = end_time - start_time
    overhead_time = start_time - start_overhead_time
    print("Parralel processing time:")
    print(parallel_time)
    print("Overhead of setting up cluster time:")
    print(overhead_time)
    
    return(resu)
  }
}

matches_process <- function(data) {
  df = data %>%
    bind_rows(.id = "rownr") %>%
    left_join(parsed_names,
              by=c("rownr"="rownr"))
  
  df %<>%
    filter(grepl("exact_match",reasons)|
             (grepl("firstname_match",reasons)&
                grepl("surname_match",reasons))|
             (grepl("surname_match",reasons)&
                (grepl("initials_match",reasons)|
                   grepl("initials_outer_match",reasons))))
  
  return(df)
}

puerki <- function(ids,
                   which = "ids",
                   stepcount = T,
                   agent = "collector_matching") {
  require(httr)
  
  steps = seq(1,
              dim(ids)[1],
              by=50)
  
  resu.r = list()
  j=1
  for (i in steps) {
    if (i == steps[length(steps)]) {
      jump = dim(ids)[1] - i
    } else {
      jump = 49
    }
    tst = paste(ids[[which]][i:(i+jump)],
                collapse="|")
    resu = httr::GET(url = paste0("https://www.wikidata.org/w/api.php",
                                  "?action=wbgetentities&ids=",
                                  tst,
                                  "&format=json"),
                     httr::user_agent(agent))
    resu.r[[j]] = httr::content(resu,
                                type="application/json")
    if (stepcount) {print(j)}
    j=j+1
  }
  return(resu.r)
}

retrieve_claims <- function(result,
                            cache = NULL,
                            index = "data/propcache/index.txt") {
  if (!is.null(cache)) {
    result %<>% 
      filter(!id%in%names(cache),
             !duplicated(id))
  } else if (file.exists(index)) {
    index_f = read_tsv(index)
    result %<>%
      filter(!id%in%index_f$key)
  }
  
  if (dim(result)[1]>0) {
    claims = puerki(result,
                    which = "id",
                    stepcount = T)
    
    temp = lapply(claims, function(x) x$entities)
    
    cache = do.call(c,temp)
  }
  
  return(cache)
}

save_claims <- function(cache,
                        index = "data/propcache/index.txt") {
  require(jsonlite)
  
  if (!dir.exists("data/propcache")) {
    dir.create("data/propcache")
  }
  if (!dir.exists("data/propcache/raw")) {
    dir.create("data/propcache/raw")
  }
  
  if (file.exists(index)) {
    index_f = read_tsv(index)
    cache = cache[!names(cache)%in%index_f$key]
    extant = max(index_f$loc)
  } else {
    extant = 0
  }
  
  if (length(cache)==0) {
    return("All claims already cached on disk.")
  }
  
  locs = rep(1:(length(cache)/100),
             each=100)
  if (length(cache) %% 100 != 0) {
    locs %<>%
      c(rep(max(locs)+1,
            times = length(cache) %% 100))
  }
  new_index = tibble(key = names(cache),
                 loc = locs) %>%
    mutate(loc = loc + extant)
  
  step = 1
  for (i in 1:max(locs)) {
    j = toJSON(cache[step:min(step+99,
                              length(cache))],
               auto_unbox = T,
               pretty = T)
    write(j,
          paste0("data/propcache/raw/",
                 i+extant,
                 ".json"))
    
    print(step)
    step = step + 100
  }
  
  if (extant > 0) {
    new_index %<>%
      rbind(index_f)
  }
  
  write_tsv(new_index,"data/propcache/index.txt")
  return(new_index)
}

get_claims_from_cache <- function(ids,
                                  props,
                                  cache = NULL,
                                  index = "data/propcache/index.txt") {
  require(jsonlite)
  
  start_time = Sys.time()
  index_f = read_tsv(index,
                     show_col_types = F)
  if (!is.null(cache)) {
    index_f %<>%
      filter(!key%in%names(cache))
  }
  toread = filter(index_f,
                  key%in%ids) %>%
    count(loc) %>%
    pull(loc)
  
  resu = list()
  if (length(toread) > 0) {
    for (i in toread) {
      res = fromJSON(paste0("data/propcache/raw/",
                            i,
                            ".json"))
      extracted = lapply(res, 
                         function(x) extract_props(x,
                                                   props))
      resu = c(resu,
               extracted)
    }
  }
  
  if (!is.null(cache)) {
    
    resu2 = lapply(cache, 
                   function(x) extract_props(data = x,
                                             props = props,
                                             auto_unbox = F))
    
    if (length(resu) == 0) {
      resu = resu2
    } else {
      resu %<>%
        c(resu2)
    }
  }
  
  end_time = Sys.time()
  parallel_time = end_time - start_time
  print(parallel_time)
  
  return(resu)
}

extract_props <- function(data,
                          props,
                          auto_unbox = T) {
  new = list()
  for (i in props) {
    if (i %in% names(data$claims)) {
      if (auto_unbox) {
        new[i] = data$claims[[i]]$mainsnak$datavalue$value$time[1]
      } else {
        new[i] = data$claims[[i]][[1]]$mainsnak$datavalue$value$time[1]
      }
    }
  }
  return(new)
}

attach_claims <- function(df,
                          props,
                          cache = NULL) {
  ids = df %>%
    count(id) %>%
    pull(id)
  
  wd_content = get_claims_from_cache(ids,
                                     c("P569","P570"),
                                     cache)
  
  for (i in props) {df[i] = NA}
  
  for (i in 1:dim(df)[1]) {
    for (j in props) {
      if (!is.null(wd_content[[df$id[i]]][[j]])) {
        df[i,j] = wd_content[[df$id[i]]][[j]]
      }
    }
  }
  
  return(df)
}

extract_year <- function(df,
                         props) {
  for (i in props) {
    df %<>%
      mutate(!!i := as.numeric(substr(!!sym(i),2,5)))
  }
  
  return(df)
}

date_filter <- function(data) {
  data %<>%
    mutate(year1 = as.numeric(year1),
           year2 = as.numeric(year2)) %>%
    filter((is.na(year1)|is.na(`P569`)|year1 > `P569`),
           (is.na(year2)|is.na(`P569`)|year2 > `P569`),
            (is.na(year1)|is.na(`P570`)|year1 <= `P570`),
             (is.na(year2)|is.na(`P570`)|year2 <= `P570`))
  return(data)
}
