# Check if all required packages are installed. If not, install them.
source("src/pkg.R")
pkgLoad()

# Load parameters for the workflow
library(ini)
config = read.ini("config.ini")

# Import the person name strings to match and parse the names for later testing
source("src/extract_strings.R")
## Keep the data in memory to connect the matched strings back to specimens
## after matching
data = extract_strings(path = config$source$data, # data file location
                       columns_list = config$source$columns, # properties to import
                       property = config$source$property, # property with the names
                       data_type = config$source$data_type) # type of data file

## Parse the name strings into first, last name, initials and
## try to interpret different syntaxes and teams using the dwc_agent ruby gem
parsed_names = parse_strings(data,
                             config$source$property)

# Fetch a subset of Wikidata items for persons that are considered potential
# collectors or determiners of specimens
source("src/create_wd_space.R")
## The subset can be acquired by a set of SPARQL queries (in data/sparql)
## or from file. Items are fetched along with (English) labels and aliases
## and these are also parsed, but not using the dwc_agent gem.
wikiResults = create_wd_space(config$source$wikifile)

# Match the sourced strings to the Wikidata subset
source("src/matching.R")
## Determine the set of cores that can be used on this machine for
## parallel computing
cores = assess_cores(config$matching$cores)

## Perform the matching
matching_results = threading(data = parsed_names,
                             f = match_string,
                             num_threads = cores,
                             req_args = "wikiResults")

## Add scores to the matching and group them per wikidata ID
## rmode indicates which matches to keep:
### "all" = default, but this does exclude matches only on initials
### "best" = the highest ranked result
### "cut" = specify a number of results to keep as a maximum
rmode = config$matching$rmode 
validated_results = threading(data = matching_results,
                              f = match_validate,
                              num_threads = cores,
                              req_args = "rmode")

## Filter the matches by a set of rules
## Also convert to a tibble for easier exporting of results
processed_results = matches_process(validated_results)

## Download claims from all Wikidata items linked to at least one name string
## and save them to your disk so you don't need to redo this step every time
cache = processed_results %>%
  count(id) %>%
  retrieve_claims()
cache %>% save_claims()

## Take the years of birth/death from the Wikidata claims and attach them
## then filter on date clashes between the specimens and Wikidata items
props = c("P569","P570")
processed_results %<>%
  attach_claims(props,cache) %>%
  extract_year(props) %>%
  date_filter()

# Export the matched names into the specified export format
source("src/export.R")
processed_results %>%
  export(data = data,
         foldername = config$source$data,
         property = config$source$property,
         export_type = config$export)
