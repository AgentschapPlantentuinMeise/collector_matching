source("src/pkg.R")
pkgLoad()

library(ini)
config = read.ini("config.ini")

source("src/extract_strings.R")
data = extract_strings(config$default$dwc_folder,
                       config$default$columns,
                       config$default$dwc_property)

parsed_names = parse_strings(data)

source("src/create_wd_space.R")
wikiResults = create_wd_space(config$default$wikifile)

source("src/matching.R")
cores = assess_cores(config$default$cores)

matching_results = threading(data = parsed_names,
                             f = match_string,
                             num_threads = cores,
                             req_args = "wikiResults")

rmode = config$default$rmode
validated_results = threading(data = matching_results,
                              f = match_validate,
                              num_threads = cores,
                              req_args = "rmode")

processed_results = matches_process(validated_results)

cache = processed_results %>%
  count(id) %>%
  retrieve_claims()
cache %>% save_claims()

props = c("P569","P570")

processed_results %<>%
  attach_claims(props,cache) %>%
  extract_year(props) %>%
  date_filter()

source("src/export.R")
processed_results %>%
  export(data = data,
         foldername = config$default$dwc_folder,
         export_type = config$default$output,
         qid = config$default$institution_qid)
