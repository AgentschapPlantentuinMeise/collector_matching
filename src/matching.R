matchString <- function(name,
                        wiki,
                        lname_cut = 2) {
  true_match = wiki %>%
    filter(itemLabel == name$parsed) %>%
    mutate(reason = "exact_match")
  
  lname_match = wiki %>%
    filter(surname == name$surname) %>%
    mutate(reason = "surname_match")
  
  lname_fuzzy_match = wiki %>%
    filter(agrepl(name$surname,
                  surname)) %>%
    filter(nchar(surname)-lname_cut < nchar(name$surname)) %>%
    mutate(reason = "surname_fuzzy_match")
  
  resu = true_match %>%
    rbind(lname_match) %>%
    lname_fuzzy_match
  
  return(resu)
}