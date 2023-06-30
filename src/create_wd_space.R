library(tidyverse)
library(magrittr)

source("src/wd_functions.R")

queries = readSPARQL()

raw = getSPARQL(queries = queries,
                logging=T)

wikiResults = joinSPARQL(raw)

#write_tsv(wikiResults,"wikiresults.txt",na="")

#wikiResults = read_tsv("wikiresults.txt")

wikiResults %<>% process_wd()

aliases = wikiResults %>%
  aliases_wd()
