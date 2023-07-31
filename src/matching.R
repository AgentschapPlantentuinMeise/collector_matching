matchString <- function(name,
                        wiki,
                        alias=NULL,
                        lname_cut = 2) {
  #initialize result list
  matches = wiki[1,] %>%
    select(itemLabel,id) %>%
    mutate(reason = "nonewhatsoever")
  
  #exact full match between parsed name and wikidata item label
  matches = wiki %>%
    filter(itemLabel == name$parsed) %>%
    select(itemLabel,id) %>%
    mutate(reason = "exact_match") %>%
    rbind(matches,.)
  
  #exact match of parsed surname and presumed wikidata item surname from label
  matches = wiki %>%
    filter(surname == name$surname) %>%
    select(itemLabel,id) %>%
    mutate(reason = "surname_match") %>%
    rbind(matches,.)
  
  #exact match of parsed first name and presumed wikidata item first name from label
  matches = wiki %>%
    filter(fname == name$fname) %>%
    select(itemLabel,id) %>%
    mutate(reason = "firstname_match") %>%
    rbind(matches,.)
  
  #fuzzy match of parsed surname into any presumed wikidata item surname from label
  #max distance between both str lengths set by lname_cut
  #default is 2, so surname in wikidata can only be 1 char longer at most
  matches = wiki %>%
    filter(agrepl(name$surname,
                  surname)) %>%
    filter(nchar(surname)-lname_cut < nchar(name$surname)) %>%
    select(itemLabel,id) %>%
    mutate(reason = "surname_fuzzy_match") %>%
    rbind(matches,.)
  
  #exact match of the interpreted initials
  matches = wiki %>%
    filter(initials == name$initials) %>%
    select(itemLabel,id) %>%
    mutate(reason = "initials_match") %>%
    rbind(matches,.)
  
  if (!is.null(alias)) {
    matches = alias %>%
      filter(itemLabel == name$parsed) %>%
      select(itemLabel,id) %>%
      mutate(reason = "alias_match") %>%
      rbind(matches,.)
    
    matches = alias %>%
      filter(initials == name$initials) %>%
      select(itemLabel,id) %>%
      mutate(reason = "alias_initials_match") %>%
      rbind(matches,.)
  }
  
  matches = matches[-1,]
  
  return(matches)
}

match_validate <- function(result,
                           mode = "best",
                           cut,
                           minscore = 10) {
  ranking = tibble(
    reason = c("exact_match",
               "surname_match",
               "firstname_match",
               "surname_fuzzy_match",
               "initials_match",
               "alias_match",
               "alias_initials_match"),
    value = c(1000,
              100,
              100,
              10,
              1,
              1000,
              1)
  )
  
  result_grouped = result %>%
    left_join(ranking,by="reason") %>%
    group_by(id) %>%
    summarize(reasons = paste(reason,collapse = "|"),
              labels = paste(itemLabel,collapse = "|"),
              score = sum(value)) %>%
    filter(score >= minscore) %>%
    arrange(desc(score))
  
  nomatch = tibble(id = NA,
                   reasons = NA,
                   labels = NA,
                   score = 0)
  
  if (mode == "best") {
    if (dim(result_grouped)[1] > 0) {
      return(result_grouped[1,])
    } else {
      return(nomatch)
    }
  } else if (mode == "cut") {
    if (!is.null(cut)) {
      return(result_grouped[1:min(cut,
                                  dim(result_grouped)[1])])
    } else {
      stop("Cut parameter not specified.")
    }
  } else if (mode == "all") {
    return(result_grouped)
  }
}

