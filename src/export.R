export <- function(match_results,
                   data,
                   property,
                   foldername,
                   export_type) {
  
  if (export_type$dwc_attribution == "true") {
    match_results %>%
      export_to_dwc_attribution(data,
                                property,
                                foldername,
                                "attribution")
  }
  
  if (export_type$ambiguous == "true") {
    match_results %>%
      ambiguous_results(omit = F) %>%
      export_to_dwc_attribution(data,
                                property,
                                foldername,
                                "ambiguous-attribution")
  }
  
  if (export_type$fst == "true") {
    match_results %>%
      save_fst(foldername)
  }
  
  if (export_type$quickstatements == "true") {
    match_results %>%
      make_quickstatements(data,
                           foldername,
                           export_type$institution_qid)
  }
}

export_to_dwc_attribution <- function(match_results,
                                      data,
                                      property,
                                      foldername,
                                      export_type) {
  match_results %<>%
    right_join(data,
               by = c("ori" = property),
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
  
  filename = foldername %>%
    generate_filename(export_type,
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
  
  dir = type %>%
    paste0("data/output/",.)
  
  foldername %<>%
    gsub("/occurrence.txt","",.,fixed = T) %>%
    gsub(".*/","",.) %>%
    paste0(dir,
           "/",
           .,
           "_",
           timestamp,
           ".",
           extension)
  
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  
  return(foldername)
}

save_fst <- function(df,
                     foldername) {
  require(fst)
  filename = foldername %>%
    generate_filename("fst",
                      "fst")
  write_fst(df,filename)
}

ambiguous_results <- function(match_results,
                              omit) {
  ambiguous = match_results %>%
    mutate(team_id = paste(parsed,
                           displayOrder,
                           sep="_")) %>%
    filter(duplicated(team_id)) %>%
    select(-team_id)
  if (omit) {
    match_results %<>%
      filter(!parsed%in%ambiguous$parsed)
  } else {
    match_results %<>%
      filter(parsed%in%ambiguous$parsed)
  }
  return(match_results)
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
  
  new = match_results %>%
    ambiguous_results(omit = T) %>%
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
  
  write_tsv(new,
            filename,
            escape="none")
}