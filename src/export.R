export <- function(match_results,
                   data,
                   foldername,
                   export_type,
                   qid) {
  if (export_type == "dwc_attribution") {
    export_to_dwc_attribution(match_results,
                              data,
                              foldername)
  }
  
  if (export_type == "fst") {
    save_fst(match_results,
             foldername)
  }
  
  if (export_type == "quickstatements") {
    make_quickstatements(match_results,
                         data,
                         foldername,
                         qid)
  }
}

export_to_dwc_attribution <- function(match_results,
                                      data,
                                      foldername) {
  match_results %<>%
    right_join(data,
               by=c("ori" = "recordedBy"),
               relationship = "many-to-many") %>%
    rename(alternateName = itemLabel,
           verbatimName = ori,
           name = parsed) %>%
    mutate(identifier = paste0("http://www.wikidata.org/entity/",
                               id),
           agentIdentifierType ="wikidata",
           agentType = "Person",
           action = "collected",
           attributionRemarks = paste0("Score: ",
                                       score,
                                       "; Reasons: ",
                                       reasons)) %>%
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
  
  if (!dir.exists("data/output/attribution")) {
    dir.create("data/output/attribution")
  }
  filename = foldername %>%
    generate_filename("attribution",
                      "txt")
  write_tsv(match_results,filename)
}

generate_filename <- function(foldername,
                              type,
                              extension) {
  timestamp = Sys.time() %>%
    as.character() %>%
    gsub("\\..*","",.) %>%
    gsub(":",".",.) %>%
    gsub(" ","_",.)
  
  foldername %<>%
    gsub(".*/","",.) %>%
    paste0("data/output/",
           type,
           "/",
           .,
           "_",
           timestamp,
           ".",
           extension)
  return(foldername)
}

save_fst <- function(df,
                     foldername) {
  require(fst)
  if (!dir.exists("data/output/fst")) {
    dir.create("data/output/fst")
  }
  filename = foldername %>%
    generate_filename("fst",
                      "fst")
  write_fst(df,filename)
}

read_fst <- function(filename) {
  require(fst)
  
  df = read_fst(paste0("data/output/fst/",filename))
  
  return(df)
}

make_quickstatements <- function(match_results,
                                 data,
                                 foldername,
                                 qid) {
  specimens = match_results %>%
    right_join(data,
               by = c("ori" = "recordedBy"),
               relationship = "many-to-many",
               keep = T) %>%
    group_by(recordedBy) %>%
    summarize(specimen_id = first(occurrenceID))
  
  ambiguous = match_results %>%
    filter(duplicated(parsed))
  
  new = match_results %>%
    filter(parsed%in%ambiguous$parsed) %>%
    left_join(specimens,
              by = c("ori" = "recordedBy"),
              relationship = "many-to-many") %>%
    filter(!duplicated(id)) %>%
    mutate(id = gsub(".*/","",id),
           coll_items = "P11146",
           qid = qid,
           ref_url = "S854",
           specimen_id = paste0("\"",
                                specimen_id,
                               "\"")) %>%
    select(id,
           coll_items,
           qid,
           ref_url,
           specimen_id)
  
  filename = foldername %>%
    generate_filename("qs",
                      "txt")
  
  if (!dir.exists("data/output/qs")) {
    dir.create("data/output/qs")
  }
  
  write_tsv(new,
            filename,
            escape="none")
}