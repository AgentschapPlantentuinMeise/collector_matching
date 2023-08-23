library(tidyverse)
library(magrittr)

source("src/matching.R")

#run extract_strings.R and create_wd_space.R first

#do the matching
#small batches for testing
c1 = 20001
c2 = 30000
tryout = dates[c1:c2,]

library(parallel)
cores = detectCores()

if (is.na(cores)) {
  cores = 2
}

#using i7-10700k:
##8threads: 21.6min for 10k names T=70°C, CPU usage = 50%
##16threads: 15.2min for 10k names T=75°C, CPU usage = 100%

#match every string to wikidata items, returning all possible matches
matching_results3 = threading(data = dates,
                             f = matchString,
                             num_threads = cores,
                             req_args = c("wikiResults","aliases"))

#validate each set of matches
#default method is first result only
rmode = "all"
best2 = threading(data = new,
                 f = match_validate,
                 num_threads = cores,
                 req_args = "rmode")
#for 10k: 32s
#for 57.9k: 7.7min
dates %<>%
  rownames_to_column("rownr")

best_t = best2 %>%
  bind_rows(.id = "rownr") %>%
  left_join(dates,
            by=c("rownr"="rownr"))

ids_to_cache = best_t %>%
  filter(score>10) %>%
  count(id)
