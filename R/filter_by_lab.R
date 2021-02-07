filter_by_lab <- function(.data, filter = TRUE) {
  if (filter) {
    labs <- c("AEL", "BAPTIST", "CCHS", "POPLAR", "UT")

    .data %>%
      dplyr::filter(!.data[[".lab_tmp_"]] %in% labs) %>%
      dplyr::select(-".lab_tmp_")
  } else {
    .data
  }
}

std_lab_names <- function(string) {
  string %>%
    stringi::stri_trans_general("Any-Latin;Latin-ASCII") %>%
    stringr::str_to_upper() %>%
    stringr::str_remove_all("[']+") %>%
    stringr::str_replace_all(pattern = "[^A-Z0-9 ]+", replacement = " ") %>%
    stringr::str_squish()
}

std_labs_ael <- function(string) {
  ael <- c("(^|.+ )AEL($| .+)", "AMERESOLABCENTCNTR") %>%
    paste0(collapse = "|")
  stringr::str_replace(string, ael, replacement = "AEL")
}

std_labs_baptist <- function(string) {
  baptist <- ".*BAPTIST.*"
  stringr::str_replace(string, baptist, replacement = "BAPTIST")
}

std_labs_poplar <- function(string) {
  poplar <- ".*POPLAR HEALTH(CARE)?.*"
  stringr::str_replace(string, poplar, replacement = "POPLAR")
}

std_labs_ut <- function(string) {
  ut <- "(^|.+ )UT(MG)?($| .+)"
  stringr::str_replace(string, ut, replacement = "UT")
}

std_labs_labcorp <- function(string) {
  labcorp <- ".*LABCORP.*"
  stringr::str_replace(string, labcorp, replacement = "LABCORP")
}

std_labs_quest <- function(string) {
  quest <- "(^|.* )QUEST($| .*)"
  stringr::str_replace(string, quest, replacement = "QUEST DIAGNOSTICS")
}

std_labs_methodist <- function(string) {
  methodist <- "^METHODIST.*"
  stringr::str_replace(string, methodist, replacement = "METHODIST")
}

std_labs_mm <- function(string) {
  mm <- "^MM .*"
  stringr::str_replace(string, mm, replacement = "MM")
}

std_labs_mmg <- function(string) {
  mmg <- "^MMG .*"
  stringr::str_replace(string, mmg, replacement = "MMG")
}
