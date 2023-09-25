# path = commandArgs(trailingOnly = T)
# 
# if (length(path) == 0) {
#   gbiffolder = "src/gbif-data-folder.txt"
#   if (file.exists(gbiffolder)) {
#     path = readLines(gbiffolder)
#   } else {
#     path = list.dirs("data/gbif sets",
#                      recursive = F)[1]
#   }
# }
# 
# columns = readLines("src/gbif-columns.txt")
# 
# parsed_names = extract_strings(path,
#                                columns)

extract_strings <- function(path,
                            columns_list) {
  require(tidyverse)

  columns = readLines(columns_list)
  
  data = read_tsv(paste0(path,"/occurrence.txt"),
                  quote="",
                  col_select = all_of(columns),
                  col_types = cols(.default = "c"))
  return(data) 
}

parse_strings <- function(data) {
  require(magrittr)
  source("src/base_parsing.R")
  
  parsed_names = data %>%
    count(recordedBy) %>%
    pull(recordedBy) %>%
    parse_names() %>%
    interpret_strings(colname = "parsed") %>%
    left_join(select(data,
                     recordedBy,
                     year),
              by=c("ori"="recordedBy"),
              relationship = "many-to-many") %>%
    group_by(parsed,
             fname,
             surname,
             initials,
             outer_initials,
             displayOrder) %>%
    summarize(year1 = min(year),
              year2 = max(year),
              ori = first(ori)) %>%
    rownames_to_column("rownr")
  
  return(parsed_names)
}