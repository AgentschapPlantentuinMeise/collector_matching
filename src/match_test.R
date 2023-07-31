source("src/matching.R")

#run extract_strings.R and create_wd_space.R first
#do the matching

matching_results = threading(data = dates,
                             f = matchString,
                             num_threads = 8,
                             req_args = c("wikiResults","aliases"))

best = threading(data = matching_results,
                 f = match_validate,
                 num_threads = 8)

best %<>%
  bind_rows() %>%
  mutate(parsed = dates$parsed[1:length(resu)],
         ori = dates$ori[1:length(resu)])
