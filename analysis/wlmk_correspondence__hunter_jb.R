
wlmk_hjb_fonds <- read_csv("../his4360-lac-search-results/data/out/search__wlmk_correspondence__hunter_jb.csv") %>%
  clean_names

wlmk_hjb_details <- wlmk_hjb_fonds %>%
  retrieve_records() %>%
  extract_record_details() %>%
  remove_system_text_from_details() %>%
  select(-html) %>%
  remove_extra_columns() %>%
  arrange(id_number)

wlmk_hjb_details %>%
  select(id_number, detail_date_s, detail_subject_heading, detail_additional_name_s, ordering_conditions_of_access) %>%
  mutate(correspondents = map(detail_additional_name_s, str_split, pattern = fixed("Correspondent:"))) %>%
  select(-detail_additional_name_s) %>%
  unnest(c(correspondents)) %>%
  unnest(c(correspondents)) %>%
  mutate_at(c("detail_subject_heading", "correspondents", "ordering_conditions_of_access"), trimws) %>%
  filter(correspondents != "")

