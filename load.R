library(tidyverse)
library(rvest)
library(janitor)

library(helpers)

source("lib/load-helpers.R")

search_electronic_results <- read_csv("../his4360-lac-search-results/data/out/search_electronic_results.csv") %>%
  clean_names()

search_electronic_results %>%
  filter(hierarchy_level == "Fonds / Collection") %>%
  slice(1:28) %>%
  select(id_number, display_url) %>%
  mutate(html = map2(id_number, display_url, read_html_from_disk_or_pull)) %>%
  mutate(details = map(html, extract_structured_details)) %>%
  unnest(c(details)) %>%
  widen_records()

search_electronic_results %>%
  filter(hierarchy_level == "Fonds / Collection") %>%
  slice(30:45) %>%
  select(id_number) %>%
  mutate(display_url = paste0("https://www.bac-lac.gc.ca/eng/CollectionSearch/Pages/record.aspx?app=FonAndCol&IdNumber=", id_number)) %>%
  mutate(html = map2(id_number, display_url, read_html_from_disk_or_pull)) %>%
  mutate(details = map(html, extract_structured_details)) %>%
  unnest(c(details)) %>%
  widen_records()



government_fonds <- read_csv("../his4360-lac-search-results/data/out/browse_government_fonds.csv") %>%
  clean_names()

government_fonds %>%
  filter(hierarchy_level == "Fonds / Collection") %>%
  filter(language_of_cataloging == "eng") %>%
  filter(str_detect(title, "electronic"))



government_fonds %>%
  filter(hierarchy_level == "Fonds / Collection") %>%
  filter(language_of_cataloging == "eng") %>%
  slice(1:5) %>%
  retrieve_record_details
  

##
## ideas:
## x use the raw HTML (in `html` column) as the data for a function that extracts the
##   HTML-formatted `detail_extent` info, breaking it into a list. [done, see below]
##

search_electronic_results %>%
  filter(hierarchy_level == "Fonds / Collection") %>%
  slice(1:28) %>%
  select(id_number, display_url) %>%
  mutate(html = map2(id_number, display_url, read_html_from_disk_or_pull)) %>%
  mutate(extent = map(html, function(html) {
    html %>%
      html_node(xpath = '//dt[contains(text(),"Extent:")]/following-sibling::dd[1]') %>%
      as.character %>%
      str_remove(pattern = fixed('<dd class="row-value">')) %>%
      str_remove(pattern = fixed('</dd>')) %>%
      str_split(pattern = fixed('<br>'))
  })) %>%
  unnest(c(extent)) %>%
  unnest(c(extent)) %>%
  mutate(extent = trimws(extent)) %>%
  filter(extent != "")
