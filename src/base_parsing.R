parse_names <- function(names) {
  writeLines(names,"data/names.txt")
  
  system("ruby src/agent_parse.rb")
  
  parsed_names = readLines("data/output/names_parsed.txt") %>% 
    tibble(raw = .) %>%
    mutate(ori = str_extract(raw,
                             "^([^\t]+)"),
           parsed = gsub("^.*?\t",
                         "",
                         raw)) %>%
    filter(parsed!="") %>%
    separate_rows(parsed,
                  sep="\t") %>%
    filter(!duplicated(parsed),
           !is.na(parsed),
           parsed!="")
  
  return(parsed_names)
}

interpret_strings <- function(data,
                              colname,
                              inc_surname=T,
                              inc_fname=T,
                              inc_initials=T) {
  if (inc_surname) {
    data %<>%
      mutate(surname = gsub("^(.*[\\s])",
                            "",
                            eval(sym(colname)),
                            perl=T))
  }
  if (inc_fname&inc_surname) {
    data %<>%
      mutate(fname = gsub("\\s.*",
                          "",
                          eval(sym(colname))),
             fname = ifelse(grepl(".",
                                  fname,
                                  fixed=T)|
                              surname == fname,
                            NA,
                            fname))
  }
  if (inc_initials) {
    data %<>%
      mutate(initials = gsub("\'",
                             "",
                             eval(sym(colname)),
                             fixed=T),
             initials = gsub("\"",
                             "",
                             initials,
                             fixed=T),
             initials = gsub("(?<!\\s).",
                             "",
                             initials,
                             perl=T),
             initials = paste0(
               substr(eval(sym(colname)),1,1),
               initials),
             initials = toupper(initials))
  }
  return(data)
}