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
matching_results3 = threading(data = tryout,
                             f = matchString,
                             num_threads = cores,
                             req_args = c("wikiResults","aliases"))

#validate each set of matches
#default method is first result only
rmode = "all"
best2 = threading(data = matching_results,
                 f = match_validate,
                 num_threads = cores,
                 req_args = "rmode")
s#for 10k: 32s

#unlist and attach the original source (and parsed) strings again
best2 %<>%
  bind_rows() %>%
  mutate(parsed = dates$parsed[c1:c2],
         ori = dates$ori[c1:c2])
