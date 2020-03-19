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

## Looks for the record file on disk. If present, loads it. Else, loads from web (and saves a copy to disk).
read_html_from_disk_or_pull <- function(id_number, display_url, save_copy_locally = TRUE) {
  record_file_path <- paste0("data/source/record-pages/", id_number, ".html")
  
  if (fs::file_exists(record_file_path)) {
    return(read_html(record_file_path))
  }
  
  ## Doesn't exist, pull from web.
  html_to_return <- read_html(display_url)
  
  if (save_copy_locally) {
    html_to_return %>% write_html(file = record_file_path)
  }
  
  return(html_to_return)
}
