library(tidyverse)
library(rvest)
library(janitor)

library(helpers)

urls <- c(
  "http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=120179&lang=eng",
  "http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=132998&lang=eng",
  "http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=1284188&lang=eng",
  "http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=4938680&lang=eng",
  "http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=4589379&lang=eng"
)

records <- tibble(
  url = urls
)

r120179 <- read_html("http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=120179&lang=eng")
r132998 <- read_html("http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=132998&lang=eng")
r1284188 <- read_html("http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=1284188&lang=eng")
r4938680 <- read_html("http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=4938680&lang=eng")
r4589379 <- read_html("http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=4589379&lang=eng")

extract_section_key_values <- function(ctx, sectionId) {
  tibble(
    section = sectionId,
    key = ctx %>%
      html_nodes(paste(sectionId, "dt")) %>%
      html_text(),
    value = ctx %>%
      html_nodes(paste(sectionId, "dd")) %>%
      html_text()
  )
}

extract_structured_details <- function(page) {
  extract_section_key_values(page, "#briefSection") %>%
    rbind(
      extract_section_key_values(page, "#detailSection"),
      extract_section_key_values(page, "#orderingSection")
    )
}

widen_records <- function(records) {
  records %>%
    mutate(section = str_remove(section, fixed("#"))) %>%
    mutate(section = str_remove(section, fixed("Section"))) %>%
    pivot_wider(names_from = c(section, key), values_from = value) %>%
    clean_names()
}



r4589379 %>%
  extract_structured_details %>%
  widen_records()

list(
  r120179,
  r132998,
  r1284188,
  r4938680,
  r4589379
) %>%
  map_dfr(extract_structured_details, .id = "id") %>%
  widen_records()

z <- records %>% slice(1:2) %>% pull(url) %>% lapply(read_html)
z %>% map_dfr(extract_structured_details, .id = "id") %>%
  widen_records()


search_electronic_results <- read_csv("../his4360-lac-search-results/data/out/search_electronic_results.csv") %>% clean_names
zz <- search_electronic_results %>%
  filter(hierarchy_level == "Fonds / Collection") %>%
  slice(1:5) %>%
  pull(display_url) %>%
  lapply(read_html)
zz %>% map_dfr(extract_structured_details, .id = "id") %>%
  widen_records()
zz %>% map_dfr(extract_structured_details, .id = "id") %>%
  widen_records() %>%
  write_csv("data/out/zz.csv")

  
