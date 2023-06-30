library(tidyverse)
library(magrittr)

path = "../gbif-sets/0195853-230224095556074"

data = read_tsv(paste0(path,"/occurrence.txt"),
                quote="",
                col_types = cols(.default = "c"))

str = count(data,recordedBy)
writeLines(pull(str,recordedBy),"data/names.txt")

system("ruby src/agent_parse.rb")

parsed_names = readLines("data/output/names_parsed.txt") %>% 
  tibble(raw = .) %>%
  mutate(ori = str_extract(raw,
                           "^([^\t]+)"),
         parsed = gsub("^.*?\t",
                       "",
                       raw))

parsed_names2 = filter(parsed_names,
               parsed!="") %>%
  separate_rows(parsed,
                sep="\t")

parsed_names3 = parsed_names2 %>%
  filter(!duplicated(parsed))

dates = left_join(parsed_names3,
                  select(data,recordedBy,year),
                  by=c("ori"="recordedBy")) %>%
  group_by(parsed) %>%
  summarize(year1 = min(year),
            year2 = max(year),
            ori = first(ori)) %>%
  mutate(surname = gsub("^(.*[\\s])",
                        "",
                        parsed,
                        perl=T),
         fname = gsub("([\\s].*)$",
                      "",
                      parsed,
                      perl=T),
         fname = ifelse(surname == fname,
                        NA,
                        fname))
