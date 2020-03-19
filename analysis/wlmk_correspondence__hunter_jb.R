
wlmk_hjb_fonds <- read_csv("../his4360-lac-search-results/data/out/search__wlmk_correspondence__hunter_jb.csv") %>%
  clean_names

wlmk_hjb_fonds %>%
  retrieve_records() %>%
  extract_record_details() %>%
  remove_system_text_from_details() %>%
  select(-html) %>%
  remove_extra_columns() %>%
  View()
