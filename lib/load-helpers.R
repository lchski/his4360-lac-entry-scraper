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


## NTS: way faster instead of using redirect is to just generate URL directly from ID
## ID 108977
## https://www.bac-lac.gc.ca/eng/CollectionSearch/Pages/record.aspx?app=FonAndCol&IdNumber=108977
retrieve_records <- function(records_to_lookup) {
  records_to_lookup %>%
    select(id_number) %>%
    mutate(display_url = paste0("https://www.bac-lac.gc.ca/eng/CollectionSearch/Pages/record.aspx?app=FonAndCol&IdNumber=", id_number)) %>%
    mutate(html = map2(id_number, display_url, read_html_from_disk_or_pull))
}

extract_record_details <- function(records_to_lookup) {
  records_to_lookup %>%
    mutate(details = map(html, extract_structured_details)) %>%
    unnest(c(details)) %>%
    widen_records()
}

extract_record_extents <- function(records_to_lookup) {
  records_to_lookup %>%
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
}

remove_system_text_from_details <- function(record_details) {
  record_details %>%
    mutate_if(is_character, str_remove, pattern = fixed("if ($('#jq-container-body-Subjects_content').text().length < 200) { $('#jq-container-header-Subjects_content').hide(); $('#jq-container-body-Subjects_content').show();}")) %>%
    mutate_if(is_character, str_remove, pattern = fixed("if ($('#jq-container-body-AddNames_content').text().length < 200) { $('#jq-container-header-AddNames_content').hide(); $('#jq-container-body-AddNames_content').show();}")) %>%
    mutate_if(is_character, str_remove, pattern = fixed("if ($('#jq-container-body-ConditionsOfAccess_content').text().length < 300) { $('#jq-container-header-ConditionsOfAccess_content').hide(); $('#jq-container-body-ConditionsOfAccess_content').show();}")) %>%
    mutate_if(is_character, str_remove, pattern = "^\nShow detailHide detail")
}
