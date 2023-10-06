library(ini)
config = read.ini("config.ini")

library(fst)
processed_results = read_fst(config$rebuild$filename)

source("src/extract_strings.R")
data = extract_strings(path = config$source$data, 
                       columns_list = config$source$columns, 
                       property = config$source$property, 
                       data_type = config$source$data_type) 

source("src/export.R")
library(magrittr)
processed_results %>%
  export(data = data,
         property = config$source$property,
         foldername = config$source$data,
         export_type = config$export)
