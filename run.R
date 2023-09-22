library(ini)
config = read.ini("config.ini")

source("src/extract_strings.R")

data = extract_strings(config$default$dwc_folder,
                       config$default$columns)

parsed_names = parse_strings(data)