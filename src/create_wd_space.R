library(tidyverse)
library(magrittr)

source("src/wd_functions.R")

wikiResults = joinSPARQL()

write_tsv(wikiResults,"wikiresults.txt",na="")
