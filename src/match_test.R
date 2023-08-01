source("src/matching.R")

#run extract_strings.R and create_wd_space.R first

#do the matching
tryout = dates[10001:20000,]

#match every string to wikidata items, returning all possible matches
matching_results2 = threading(data = tryout,
                             f = matchString,
                             num_threads = 8,
                             req_args = c("wikiResults","aliases"))

#validate each set of matches
#default method is first result only
rmode = "all"
best2 = threading(data = matching_results2,
                 f = match_validate,
                 num_threads = 8,
                 req_args = "rmode")

#unlist and attach the original source (and parsed) strings again
best2 %<>%
  bind_rows() %>%
  mutate(parsed = dates$parsed[1:length(resu)],
         ori = dates$ori[1:length(resu)])
