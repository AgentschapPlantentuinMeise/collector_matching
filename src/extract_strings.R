extract_strings <- function(path,
                            columns_list,
                            property,
                            data_type) {
  # path = (relative) path to where the data file(s) can be found
  # columns_list = path to a file listing colnames to import
  # dwc_property = colname which contains the name strings to match
  # data_type = format of the data file(s)
  ## "DwC-A" = a Darwin Core Archive (unzipped). occurrence.txt will be used
  ## "dissco" = a JSON document as exported from the DiSSCo sandbox
  ### !!!the dissco import is not serialized yet!!
  ### !!!the dissco JSON model may still change!!!
  require(tidyverse)
  columns = readLines(columns_list) %>%
    c(property)
  
  if (data_type == "DwC-A") {
    data = read_tsv(path,
                    quote="",
                    col_select = all_of(columns),
                    col_types = cols(.default = "c"))
  }
  if (data_type == "dissco") {
    require(jsonlite)
    raw = fromJSON(path,simplifyVector = F)
    data = tibble(!!property := sapply(raw,
                                       function(x) 
                                         x$data$attributes$originalData[[paste0("dwc:",
                                                             sym(property))]]),
                  year = sapply(raw,
                                function(x) 
                                  ifelse(!is.null(x$data$attributes$originalData$`dwc:year`),
                                         x$data$attributes$originalData$`dwc:year`,
                                         NA)),
                  occurrenceID = sapply(raw,
                                        function(x) 
                                          x$data$attributes$physicalSpecimenId),
                  gbifID = sapply(raw,
                                  function(x) 
                                    x$data$attributes$id))
  }
  return(data) 
}

parse_strings <- function(data,
                          property) {
  require(magrittr)
  source("src/base_parsing.R")
  
  parsed_names = data %>%
    count(!!sym(property)) %>%
    pull(!!sym(property)) %>%
    parse_names() %>%
    interpret_strings(colname = "parsed") %>%
    left_join(select(data,
                     !!sym(property),
                     year),
              by=c("ori" = property),
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