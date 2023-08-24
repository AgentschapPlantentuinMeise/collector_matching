library(tidyverse)
library(magrittr)

source("src/base_parsing.R")

path = commandArgs(trailingOnly = T)

if (length(path) == 0) {
  gbiffolder = "src/gbif-data-folder.txt"
  if (file.exists(gbiffolder)) {
    path = readLines(gbiffolder)
  } else {
    path = list.dirs("data/gbif sets",
                     recursive = F)[1]
  }
}

columns = readLines("src/gbif-columns.txt")

data = read_tsv(paste0(path,"/occurrence.txt"),
                quote="",
                col_select = all_of(columns),
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
