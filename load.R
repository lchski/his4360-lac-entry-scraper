library(tidyverse)
library(rvest)
library(janitor)

library(helpers)

records <- tibble(
  url = c(
    "http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=120179&lang=eng",
    "http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=132998&lang=eng",
    "http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=1284188&lang=eng",
    "http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=4938680&lang=eng",
    "http://central.bac-lac.gc.ca/.redirect?app=fonandcol&id=4589379&lang=eng"
  )
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
    extract_structured_details %>%
    mutate(section = str_remove(section, fixed("#"))) %>%
    mutate(section = str_remove(section, fixed("Section"))) %>%
    pivot_wider(names_from = c(section, key), values_from = value) %>%
    clean_names()
}

r4589379 %>%
  widen_records()
  
  
