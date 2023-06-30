querki <- function(query,h="text/csv",
                   agent="your_user_name_here",
                   logging = F) {
  require(httr)
  response <- httr::GET(url = "https://query.wikidata.org/sparql", 
                        query = list(query = query),
                        httr::add_headers(Accept = h),
                        httr::user_agent(agent))
  response_content = httr::content(response,
                                   type=h,
                                   col_types = cols(.default = "c"))
  if (logging) {print(problems(response_content))}
  return(response_content)
}

readSPARQL <- function() {
  files = list.files("data/sparql",
                     recursive = T,
                     full.names = T)
  type = str_extract(files,"data/.*/") %>%
    gsub("data/sparql/","",.) %>%
    gsub("/","",.)
  queries = tibble(filename = files,
                   query = NA,
                   type = type)
  for (i in 1:length(files)) {
    queries$query[i] = readChar(files[i],
                                file.info(files[i])$size)
  }
  return(queries)
}

getSPARQL <- function(queries = NULL,
                      agent = "collector_matching",
                      logging = F) {
  if (is.null(queries)) {
    queries = readSPARQL()
  }
  raw = list()
  for (i in 1:dim(queries)[1]) {
    raw[[i]] = querki(queries$query[i],
                      agent = agent,
                      logging = logging)
    if (!is.null(queries$type)) {
      if (queries$type[i] == "props") {
        name = gsub(".*/","",queries$filename[i]) %>%
          gsub(".txt","",.)
        raw[[i]][[name]] = name
      }
    }
  }
  return(raw)
}

joinSPARQL <- function(raw = NULL,
                       unique = T,
                       logging = F) {
  if (is.null(raw)) {
    raw = getSPARQL(logging = logging)
  }
  
  if (length(raw)>1) {
    wikiResults = merge(raw[[1]],
                        raw[[2]],
                        all.x=T,
                        all.y=T)
  } else if (length(raw)>0) {
    return(raw[[1]])
  } else {
    stop("No data found")
  }
  
  if (length(raw)>2) {
    for (i in 3:length(raw)) {
      wikiResults = merge(wikiResults,
                          raw[[i]],
                          all.x=T,
                          all.y=T)
    }
  }
  
  if (unique) {
    wikiResults %<>% 
      filter(!duplicated(item))
  }
  wikiResults$id = gsub("http://www.wikidata.org/entity/",
                              "",
                              wikiResults$item)
  return(wikiResults)
}

process_wd <- function(wikiResults,
                          inc_surname=T,
                          inc_initials=T) {
  if (inc_surname) {
    wikiResults$surname = gsub("^(.*[\\s])",
                               "",
                               wikiResults$itemLabel,
                               perl=T)
  }
  if (inc_initials) {
    wikiResults %<>%
      mutate(initials = gsub("\'",
                             "",
                             itemLabel,
                             fixed=T),
             initials = gsub("\"",
                             "",
                             initials,
                             fixed=T),
             initials = gsub("(?<!\\s).",
                             "",
                             initials,
                             perl=T),
             initials = paste0(
               substr(itemLabel,1,1),
               initials))
  }
  return(wikiResults)
}

aliases_wd <- function(wikiResults) {
  altnames = wikiResults %>%
    separate_rows(itemAltLabel,sep=", ") %>%
    select(-itemLabel) %>%
    rename(itemLabel = itemAltLabel) %>%
    select(itemLabel,id) %>%
    rbind(select(wikiResults,itemLabel,id)) %>%
    filter(!is.na(itemLabel)) %>%
    mutate(test = paste0(id,
                         itemLabel)) %>%
    filter(!duplicated(test)) %>%
    select(-test)
  return(altnames)
}

floruit_wd <- function(wikiResults) {
  wikiResults %<>%
    mutate(floruitDate1 = ifelse(!is.na(yob),
                                 NA,
                                 ifelse(!is.na(wyb),
                                        wyb,
                                        ifelse(!is.na(fly),
                                               fly,
                                               NA))),
           floruitDate2 = ifelse(!is.na(yob),
                                 NA,
                                 ifelse(!is.na(wye),
                                        wye,
                                        ifelse(!is.na(fly),
                                               fly,
                                               ifelse(!is.na(wyb),
                                                      wyb,
                                                      NA)))))
  return(wikiResults)
}

check_merged <- function(qid) {
  require(httr)
  
  query <- paste0("SELECT * WHERE { wd:", 
                  qid, 
                  " owl:sameAs ?other . }")
  
  url <- "https://query.wikidata.org/bigdata/namespace/wdq/sparql"
  response <- GET(url, 
                  query = list(query = query))
  
  data <- content(response,
                  type = "application/json")
  bindings <- data$results$bindings
  
  if (length(bindings) > 0) {
    merged_qid <- gsub("http://www.wikidata.org/entity/",
                       "",
                       bindings[[1]]$other$value)
    return(merged_qid)
  }
  
  return(NA)
}