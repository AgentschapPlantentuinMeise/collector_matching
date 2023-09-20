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
matching_results4 = threading(data = dates,
                             f = matchString,
                             num_threads = cores,
                             req_args = c("wikiResults"))

#validate each set of matches
#default method is first result only
rmode = "all"
best2 = threading(data = matching_results4,
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

miss = seq(1,dim(dates)[1]) %>% 
  tibble(id=.) %>% 
  filter(!id%in%best_t$rownr)

approved = best_t %>%
  filter(grepl("exact_match",reasons)|
           (grepl("firstname_match",reasons)&
              grepl("surname_match",reasons))|
           (grepl("surname_match",reasons)&
              (grepl("initials_match",reasons)|
                 grepl("initials_outer_match",reasons))))

ids_to_cache = approved %>%
  count(id)

cache = retrieve_claims(ids_to_cache)

wd_content = get_claims_from_cache(pull(ids_to_cache,id),
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
