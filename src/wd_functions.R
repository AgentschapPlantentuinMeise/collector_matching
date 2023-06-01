querki <- function(query,h="text/csv",agent="your_user_name_here") {
  require(httr)
  response <- httr::GET(url = "https://query.wikidata.org/sparql", 
                        query = list(query = query),
                        httr::add_headers(Accept = h),
                        httr::user_agent(agent))
  return(httr::content(response,
                       type=h,
                       col_types = cols(.default = "c")))
}

readSPARQL <- function() {
  files = list.files("data/sparql",
                     full.names = T)
  queries = tibble(filename = files,
                   query = NA)
  for (i in 1:length(files)) {
    queries$query[i] = readChar(files[i],file.info(files[i])$size)
  }
  return(queries)
}

getSPARQL <- function(queries = NULL,
                      agent = "collector_matching") {
  if (is.null(queries)) {
    queries = readSPARQL()
  }
  raw = list()
  for (i in 1:dim(queries)[1]) {
    raw[[i]] = querki(queries$query[i],agent = agent)
  }
  return(raw)
}

joinSPARQL <- function(raw = NULL,
                       unique = T) {
  if (is.null(raw)) {
    raw = getSPARQL()
  }
  
  if (is.null(raw[[2]])) {
    return(raw[[1]])
  }
  
  wikiResults = full_join(raw[[1]],
                          raw[[2]])
  if (length(raw)>2) {
    for (i in 3:length(raw)) {
      wikiResults = full_join(wikiResults,
                              raw[[i]])
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