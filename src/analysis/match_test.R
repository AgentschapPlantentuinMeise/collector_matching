library(tidyverse)
library(magrittr)
library(ini)

source("src/matching.R")

library(parallel)
cores = detectCores()

if (is.na(cores)) {
  cores = 2
}

#run extract_strings.R and create_wd_space.R first

#match every string to wikidata items, returning all possible matches
matching_results = threading(data = parsed_names,
                             f = match_string,
                             num_threads = cores,
                             req_args = "wikiResults")

#validate each set of matches
rmode = "all"
best = threading(data = matching_results,
                 f = match_validate,
                 num_threads = cores,
                 req_args = "rmode")

best_df = best %>%
  bind_rows(.id = "rownr") %>%
  left_join(parsed_names,
            by=c("rownr"="rownr"))

miss = seq(1,dim(parsed_names)[1]) %>% 
  tibble(id=.) %>% 
  filter(!id%in%best_df$rownr)

approved = fst %>%
  filter(grepl("exact_match",reasons)|
           (grepl("firstname_match",reasons)&
              grepl("surname_match",reasons))|
           (grepl("surname_match",reasons)&
              (grepl("initials_match",reasons)|
                 grepl("initials_outer_match",reasons))))

approved2 = fst2 %>%
  filter(grepl("exact_match",reasons)|
           (grepl("firstname_match",reasons)&
              grepl("surname_match",reasons))|
           (grepl("surname_match",reasons)&
              (grepl("initials_match",reasons)|
                 grepl("initials_outer_match",reasons))))

ids_to_cache = approved %>%
  count(id)

ids_to_cache2 = approved2 %>%
  count(id)

ids = rbind(ids_to_cache,ids_to_cache2) %>%
  count(id)

cache = retrieve_claims(ids_to_cache)

wd_content = get_claims_from_cache(pull(ids,id),
                                   c("P569","P570"))

approved$wdd1 = NA
approved$wdd2 = NA

for (i in 1:dim(approved)[1]) {
  if (!is.null(wd_content[approved$id[i]][[1]]$P569)) {
    approved$wdd1[i] = wd_content[approved$id[i]][[1]]$P569
  }
  if (!is.null(wd_content[approved$id[i]][[1]]$P570)) {
    approved$wdd2[i] = wd_content[approved$id[i]][[1]]$P570
  }
}

approved %<>% 
  mutate(wdyear1 = as.numeric(substr(wdd1,2,5)),
         wdyear2 = as.numeric(substr(wdd2,2,5)))

approved_amb = filter(approved,duplicated(id)) %>% 
  count(id)

ambiguous = approved %>%
  filter(id%in%approved_amb$id)

bc = save_claims(cache)

add_occ = ambiguous %>%
  right_join(data,by=c("ori" = "recordedBy"))

dwca = export_to_dwc_attribution(add_occ)
write_tsv(dwca,"meise_dwc_attribution.txt")
