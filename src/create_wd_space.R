create_wd_space <- function(wikifile) {
  require(tidyverse)
  require(magrittr)
  
  source("src/base_parsing.R")
  source("src/wd_functions.R")
  
  if (file.exists(wikifile)) {
    wikiResults = read_tsv(wikifile,
                           col_types = cols(.default = "c"))
  } else {
    queries = readSPARQL()
    
    raw = getSPARQL(queries = queries,
                    logging=T)
    wikiResults = joinSPARQL(raw)
    write_tsv(wikiResults,
              wikifile,
              na="")
  }
  
  wikiResults %<>%
    aliases_wd() %>%
    interpret_strings(colname = "itemLabel")
  
  return(wikiResults)
}