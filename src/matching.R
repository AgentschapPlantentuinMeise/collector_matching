matchString <- function(name,
                        wiki,
                        alias=NULL,
                        lname_cut = 2) {
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
  matches = wiki %>%
    filter(surname == name$surname) %>%
    select(itemLabel,id) %>%
    mutate(reason = "surname_match") %>%
    rbind(matches,.)
  
  #exact match of parsed first name and presumed wikidata item first name from label
  matches = wiki %>%
    filter(fname == name$fname) %>%
    select(itemLabel,id) %>%
    mutate(reason = "firstname_match") %>%
    rbind(matches,.)
  
  #fuzzy match of parsed surname into any presumed wikidata item surname from label
  #max distance between both str lengths set by lname_cut
  #default is 2, so surname in wikidata can only be 1 char longer at most
  matches = wiki %>%
    filter(agrepl(name$surname,
                  surname)) %>%
    filter(nchar(surname)-lname_cut < nchar(name$surname)) %>%
    select(itemLabel,id) %>%
    mutate(reason = "surname_fuzzy_match") %>%
    rbind(matches,.)
  
  #exact match of the interpreted initials
  matches = wiki %>%
    filter(initials == name$initials) %>%
    select(itemLabel,id) %>%
    mutate(reason = "initials_match") %>%
    rbind(matches,.)
  
  if (!is.null(alias)) {
    matches = alias %>%
      filter(itemLabel == name$parsed) %>%
      select(itemLabel,id) %>%
      mutate(reason = "alias_match") %>%
      rbind(matches,.)
    
    matches = alias %>%
      filter(initials == name$initials) %>%
      select(itemLabel,id) %>%
      mutate(reason = "alias_initials_match") %>%
      rbind(matches,.)
  }
  
  matches = matches[-1,]
  
  return(matches)
}

match_validate <- function(result,
                           mode = "best",#can also be "cut" or "all"
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
               "alias_match",
               "alias_initials_match"),
    value = c(1000,
              100,
              100,
              10,
              1,
              1000,
              1)
  )
  
  #collapse each result in a list of each possible wikidata qid
  #and sum the scores
  result_grouped = result %>%
    left_join(ranking,by="reason") %>%
    group_by(id) %>%
    summarize(reasons = paste(reason,collapse = "|"),
              labels = paste(itemLabel,collapse = "|"),
              score = sum(value)) %>%
    filter(score >= minscore) %>%
    arrange(desc(score))
  
  #nomatch if no match was found or only initials matched
  nomatch = tibble(id = NA,
                   reasons = NA,
                   labels = NA,
                   score = 0)
  
  if (mode == "best") {
    if (dim(result_grouped)[1] > 0) {
      return(result_grouped[1,])
    } else {
      return(nomatch)
    }
  } else if (mode == "cut") {
    if (!is.null(cut)) {
      return(result_grouped[1:min(cut,
                                  dim(result_grouped)[1])])
    } else {
      stop("Cut parameter not specified.")
    }
  } else if (mode == "all") {
    return(result_grouped)
  }
}

threading <- function(data,#list or tibble to multithread
                      f,#function to apply to data, str or function object
                      num_threads = 4,#nr of threads to use
                      req_args = NULL,#vector with names of objects to load from globalenv
                      pkg = c("tidyverse",#required packages, might be unneeded argument
                              "magrittr"),
                      srcs = "src/matching.R",#required script files, might be unneeded argument
                      dryrun = F) {#for testing
  require(parallel)
  require(doParallel)
  
  #convert data to list if it's a tibble
  if (is.tibble(data)) {
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
    print(parallel_time)
    
    return(resu)
  }
}