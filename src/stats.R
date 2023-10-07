str = data %>%
  count(!!sym(config$source$property)) %>%
  left_join(parsed_names,
            by = setNames(c("ori"),config$source$property)) %>%
  filter(!is.na(parsed))

str_u = data %>%
  count(!!sym(config$source$property))

unmatch = str %>%
  filter(!parsed%in%processed_results$parsed)

match = str %>%
  filter(parsed%in%processed_results$parsed)
match_unique = match %>%
  filter(!duplicated(recordedBy))
unmatch_unique = unmatch %>%
  filter(!recordedBy%in%match$recordedBy) %>%
  filter(!duplicated(recordedBy))

source("src/export.R")
ambiguous = processed_results %>%
  ambiguous_results(omit = F)

ambiguous_c = ambiguous %>%
  filter(!duplicated(parsed))

nonambi = processed_results %>%
  ambiguous_results(omit = T)


cia = read_tsv("data/output/P11146.tsv") %>%
  mutate(id = gsub(".*/","",item))

qs = read_tsv("data/output/qs/0167495-230224095556074_2023-10-06_22.39.59.txt")

qs_new = qs %>%
  filter(!id%in%cia$id)
