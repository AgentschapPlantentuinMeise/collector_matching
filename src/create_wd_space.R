create_wd_space <- function(wikifile) {
  require(tidyverse)
  require(magrittr)
  
  source("src/base_parsing.R")
  source("src/wd_functions.R")
  
  if (file.exists(wikifile)) {
    wikiResults = read_tsv(wikifile,
                           col_types = cols(.default = "c"))
  } else {
    queries = readSPARQL() # Read queries from the data/sparql folder
    
    # Acquire Wikidata items through SPARQL
    # Timed-out query results will be dropped completely
    raw = getSPARQL(queries = queries,
                    logging=F)
    
    # Join into a single tibble
    wikiResults = joinSPARQL(raw)
    
    # Save to file so queries don't need to be re-run every time
    # Also helps mitigate the time-out issue
    write_tsv(wikiResults,
              wikifile,
              na="")
  }
  
  # Interpret Wikidata item labels and aliases similar as the source name strings
  # The Ruby gem is not applied ad Wikidata labels are considered to be better
  # standardized than names from specimen metadata
  wikiResults %<>%
    aliases_wd() %>%
    interpret_strings(colname = "itemLabel")
  
  return(wikiResults)
}