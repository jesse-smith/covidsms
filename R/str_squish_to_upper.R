str_squish_to_upper <- function(string) {
  string %>%
    stringr::str_squish() %>%
    stringr::str_to_upper() %>%
    to_chr_ascii()
}
