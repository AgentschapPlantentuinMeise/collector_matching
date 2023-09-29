# parse name strings using the dwc_agent ruby gem
parse_names <- function(names) {
  
  writeLines(names,"data/names.txt")
  
  # run a ruby script that applies the name parsing dwc_agent gem
  # R will wait until the ruby script finished its execution
  system("ruby src/agent_parse.rb")
  
  # import the file the ruby script produced and convert it back to a tibble
  parsed_names = readLines("data/output/names_parsed.txt") %>% 
    tibble(raw = .) %>%
    mutate(ori = str_extract(raw,
                             "^([^\t]+)"),
           parsed = gsub("^.*?\t",
                         "",
                         raw),
           teamCount = nchar(gsub("[^\t]",
                               "",
                               parsed))+1,
           displayOrder = map(teamCount,
                              ~ paste(c(1:.x),
                                      collapse="\t"))) %>%
    filter(parsed!="") %>%
    separate_rows(parsed,
                  displayOrder,
                  sep="\t") %>%
    filter(!duplicated(parsed),
           !is.na(parsed),
           parsed!="",
           nchar(parsed)>3)
  
  return(parsed_names)
}

# Strings are converted for usability by the matching script
interpret_strings <- function(data,
                              colname,#name of the column to be interpreted
                              inc_surname=T,
                              inc_fname=T,
                              inc_initials=T) {
  # Surname is the last word of the space delimited string (after parsing)
  if (inc_surname) {
    data %<>%
      mutate(surname = gsub("^(.*[\\s])",
                            "",
                            eval(sym(colname)),
                            perl=T))
  }
  
  # First name is the first word of the space delimited string (after parsing)
  if (inc_fname&inc_surname) {
    data %<>%
      mutate(fname = gsub("\\s.*",
                          "",
                          eval(sym(colname))),
             fname = ifelse(grepl(".",
                                  fname,
                                  fixed=T)|
                              surname == fname|
                              nchar(fname) < 2,
                            NA,
                            fname))
  }
  
  # Initials are based on the first character of the chunks of the 
  # space or dash(-) delimited string
  # Quotes are removed so they don't end up part of the initials
  # Outer initials ignore middle names
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
             initials = gsub("(?<!\\s|-|([a-z|A-Z]\\.)).",
                             "",
                             initials,
                             perl=T),
             initials = paste0(
               substr(eval(sym(colname)),1,1),
               initials),
             initials = toupper(initials),
             initials = gsub("\\s|-","",initials),
             initials = ifelse(nchar(initials)<2,
                               NA,
                               initials),
             outer_initials = ifelse(is.na(initials),
                                     NA,
                                     paste0(substr(initials,1,1),
                                            str_sub(initials,-1,-1))))
  }
  return(data)
}