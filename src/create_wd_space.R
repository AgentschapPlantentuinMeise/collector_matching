create_wd_space <- function() {
  require(tidyverse)
  require(magrittr)
  
  source("src/base_parsing.R")
  source("src/wd_functions.R")
  
  if (file.exists("data/wikiresults.txt")) {
    wikiResults = read_tsv("data/wikiresults.txt",
                           col_types = cols(.default = "c"))
  } else {
    queries = readSPARQL()
    
    raw = getSPARQL(queries = queries,
                    logging=T)
    wikiResults = joinSPARQL(raw)
    write_tsv(wikiResults,
              "wikiresults.txt",
              na="")
  }
  
  wikiResults %<>%
    aliases_wd() %>%
    interpret_strings(colname = "itemLabel")
  
  return(wikiResults)
}