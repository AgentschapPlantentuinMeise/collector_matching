library(tidyverse)
library(magrittr)

source("src/base_parsing.R")

path= commandArgs(trailingOnly = T)

if (length(path) == 0) {
  path = "data/gbif sets/0167499-230224095556074"
}

data = read_tsv(paste0(path,"/occurrence.txt"),
                quote="",
                col_types = cols(.default = "c"))

parsed_names = data %>%
  count(recordedBy) %>%
  pull(recordedBy) %>%
  parse_names() %>%
  interpret_strings(colname = "parsed")

dates = left_join(parsed_names,
                  select(data,recordedBy,year),
                  by=c("ori"="recordedBy")) %>%
  group_by(parsed,fname,surname,initials) %>%
  summarize(year1 = min(year),
            year2 = max(year),
            ori = first(ori))
