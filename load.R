library(tidyverse)
library(rvest)
library(janitor)

library(helpers)

page <- read_html("http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=120179&lang=eng")

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

page %>% extract_structured_details

page %>%
  extract_structured_details %>%
  mutate(section = str_remove(section, fixed("#"))) %>%
  mutate(section = str_remove(section, fixed("Section"))) %>%
  pivot_wider(names_from = c(section, key), values_from = value) %>%
  clean_names() %>%
  View()
