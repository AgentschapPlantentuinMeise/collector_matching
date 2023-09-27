#make sparql query to wikidata and return csv results
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
                                   col_types = cols(.default = "c"),
                                   encoding = "UTF-8")
  if (logging) {print(problems(response_content))}
  return(response_content)
}

#read sparql queries from the data directory
readSPARQL <- function() {
  files = list.files("data/sparql",
                     recursive = T,
                     full.names = T)
  type = str_extract(files,"data/.*/") %>%
    gsub("data/sparql/","",.) %>%
    gsub("/","",.)
  ids = gsub(".*/","",files) %>%
    gsub(".txt","",.,fixed=T)
  queries = tibble(filename = files,
                   query = NA,
                   type = type,
                   id = ids)
  for (i in 1:length(files)) {
    queries$query[i] = readChar(files[i],
                                file.info(files[i])$size)
  }
  return(queries)
}

#perform a series of sparql queries
#returned is a list with results, named if a name is provided with the queries
getSPARQL <- function(queries = NULL,
                      agent = "collector_matching",
                      logging = F) {
  if (is.null(queries)) {
    queries = readSPARQL()
  }
  raw = list()
  fail = c()
  
  for (i in 1:length(queries$id)) {
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
    if (length(grep(".TimeoutException",
                    raw[[i]]$item,
                    fixed=T))>0) {
      raw[[i]] = NULL
      if (is.null(fail)) {
        fail = i
      } else {
        fail = c(fail,i)
      }
    }
  }
  if (!is.null(fail)) {
    print(paste0("Queries for ",
                 paste(queries$id[fail],
                       collapse = " AND "),
                 " timed out and were excluded."))
  }
  return(raw)
}

#joins multiple sparql query results into a single tibble
joinSPARQL <- function(raw = NULL,
                       unique = T,
                       logging = F) {
  if (is.null(raw)) {
    raw = getSPARQL(logging = logging)
  }
  
  if (!is.null(raw$fail)) {
    raw = raw[-raw$fail] %>%
      raw$fail = NULL
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

#converts wikidata results into a tibble with a row for each alias of each item
#excluding the english item label
aliases_wd <- function(wikiResults) {
  altnames = wikiResults %>%
    separate_rows(itemAltLabel,sep=", ") %>%
    select(-itemLabel) %>%
    rename(itemLabel = itemAltLabel) %>%
    select(itemLabel,id) %>%
    filter(!is.na(itemLabel)) #%>%
    # mutate(test = paste0(id,
    #                      itemLabel)) %>%
    # filter(!duplicated(test)) %>%
    # select(-test)
  altnames %<>%
    rbind(select(wikiResults,
                 itemLabel,
                 id))
  return(altnames)
}

#checks whether a qid has been merged
#returns the qid the item has been merged into if true
#and NA if no merge has happened
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