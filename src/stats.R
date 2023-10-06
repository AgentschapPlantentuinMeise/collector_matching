source("src/export.R")

str = data %>%
  count(!!sym(property)) %>%
  left_join(parsed_names,
            by = setNames(c("ori"),config$source$property)) %>%
  filter(!is.na(parsed))

unmatch = str %>%
  filter(!parsed%in%processed_results$parsed)

match = str %>%
  filter(parsed%in%processed_results$parsed)
