not_gathered3 = test %>%
  filter(!id%in%wikiResults$id)

not_gathered3$merged = NA
for (i in 1:dim(not_gathered3)[1]) {
  not_gathered3$merged[i] = check_merged(not_gathered3$id[i])
}

wikiResults2 = wikiResults %>%
  mutate(check = paste(bhl_id,
                       entom_id,
                       harv_id,
                       ipni_id,
                       article,
                       zoo_id,
                       sep=";"),
         check2 = lengths(str_extract_all(check,";NA|NA;"))) %>%
  select(-check) %>%
  filter(check2 == 5)

selected_cols = c("bhl_id",
                  "entom_id",
                  "harv_id",
                  "ipni_id",
                  "article",
                  "zoo_id")
unique_contrib = wikiResults2 %>%
  filter(id%in%test$id) %>%
  mutate(bhl_id2 = ifelse(is.na(bhl_id),NA,"bhl_id"),
         entom_id2 = ifelse(is.na(entom_id),NA,"entom_id"),
         harv_id2 = ifelse(is.na(harv_id),NA,"harv_id"),
         ipni_id2 = ifelse(is.na(ipni_id),NA,"ipni_id"),
         article2 = ifelse(is.na(article),NA,"article"),
         zoo_id2 = ifelse(is.na(zoo_id),NA,"zoo_id")) %>%
  unite("col",bhl_id2:zoo_id2,
        na.rm=T,
        remove=T) %>%
  count(col)
