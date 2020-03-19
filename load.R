library(tidyverse)
library(rvest)
library(janitor)

library(helpers)

source("lib/load-helpers.R")

government_fonds <- read_csv("../his4360-lac-search-results/data/out/browse_government_fonds.csv") %>%
  clean_names()

gov_fonds_records <- government_fonds %>%
  filter(hierarchy_level == "Fonds / Collection") %>%
  filter(language_of_cataloging == "eng") %>%
  retrieve_records()

gov_fonds_details <- gov_fonds_records %>%
  extract_record_details()
