
## all extents by RG
gov_fonds_records %>%
  extract_record_extents() %>%
  left_join(
    gov_fonds_details,
    by = c("id_number" = "id_number")
  ) %>%
  select(id_number, brief_title, extent) %>%
  arrange(brief_title) %>%
  View()

### RG extents looking for electronic
gov_fonds_records %>%
  extract_record_extents() %>%
  left_join(
    gov_fonds_details,
    by = c("id_number" = "id_number")
  ) %>%
  filter(str_detect(extent, regex("electronic|computer|data|disk|disc|kb|mb|gb|tb", ignore_case = TRUE))) %>% ## "cassette is interesting, but brings lots of video/audio
  #filter(! str_detect(extent, regex("video|audio", ignore_case = TRUE))) %>% ## we could filter on this, but pushes out some we want
  select(id_number, brief_title, extent) %>%
  arrange(brief_title) %>%
  View()


## RGs with electronic in title [NB: not every RG with electronic records has electronic in title]
government_fonds %>%
  filter(hierarchy_level == "Fonds / Collection") %>%
  filter(language_of_cataloging == "eng") %>%
  filter(str_detect(title, "electronic")) %>%
  retrieve_records() %>%
  select(id_number, html) %>%
  extract_record_extents() %>%
  left_join( ## bring in also the details themselves
    (.) %>%
      select(-extent) %>%
      distinct() %>%
      extract_record_details() %>%
      select(-html),
    by = c("id_number" = "id_number")
  ) %>%
  select(id_number, brief_title, extent)


