export_to_dwc_attribution <- function(match_results,
                                      data,
                                      foldername) {
  match_results %<>%
    right_join(data,by=c("ori" = "recordedBy")) %>%
    rename(alternateName = itemLabel,
           verbatimName = ori,
           name = parsed) %>%
    mutate(identifier = paste0("http://www.wikidata.org/entity/",id),
           agentIdentifierType ="wikidata",
           agentType = "Person",
           action = "collected",
           attributionRemarks = paste0("Score: ",score,"; Reasons: ",reasons)) %>%
    select(gbifID,
           occurrenceID,
           name,
           verbatimName,
           alternateName,
           displayOrder,
           identifier,
           agentIdentifierType,
           agentType,
           action,
           attributionRemarks)
  
  filename = foldername %>%
    gsub(".*/","",.) %>%
    paste0("output/attribution/",.,"_",Sys.time(),".txt")
  write_tsv(match_results,filename)
  
  return(match_results)
}

save_fst <- function(df,
                     foldername) {
  require(fst)
  
  filename = foldername %>%
    gsub(".*/","",.) %>%
    paste0("output/fst/",.,"_",Sys.time(),".fst")
  
  write_fst(df,filename)
}

read_fst <- function(filename) {
  require(fst)
  
  df = read_fst(paste0("output/fst/",filename))
  
  return(df)
}