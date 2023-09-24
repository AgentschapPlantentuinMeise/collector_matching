source("src/pkg.R")
pkgLoad()

library(ini)
config = read.ini("config.ini")

source("src/extract_strings.R")
data = extract_strings(config$default$dwc_folder,
                       config$default$columns)

parsed_names = parse_strings(data)

source("src/create_wd_space.R")
wikiResults = create_wd_space()

source("src/matching.R")
if (grepl("+",config$default$cores)) {
  library(doParallel)
  cores = max(detectCores(logical = F),
              gsub("+",
                   "",
                   config$default$cores))
} else {
  cores = config$default$cores
}

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

processed_results2 = processed_results %>%
  attach_claims(props,cache) %>%
  extract_year(props)

#todo: unmatched and multimatch subsets:
miss = seq(1,dim(parsed_names)[1]) %>% 
  tibble(id=.) %>% 
  filter(!id%in%best_df$rownr)

approved_amb = filter(approved,duplicated(id)) %>% 
  count(id)

ambiguous = approved %>%
  filter(id%in%approved_amb$id)