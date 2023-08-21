library(tidyverse)
library(magrittr)

source("src/base_parsing.R")
source("src/wd_functions.R")

if (file.exists("wikiresults.txt")) {
  wikiResults = read_tsv("wikiresults.txt",
                         col_types = cols(.default = "c"))
} else {
  queries = readSPARQL()
  
  raw = getSPARQL(queries = queries,
                  logging=T)
  wikiResults = joinSPARQL(raw)
  write_tsv(wikiResults,"wikiresults.txt",na="")
}

wikiResults %<>%
  interpret_strings(colname = "itemLabel")

aliases = wikiResults %>%
  aliases_wd() %>%
  interpret_strings(colname = "itemLabel")
