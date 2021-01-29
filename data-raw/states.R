states <- tidycensus::fips_codes %>%
  dplyr::distinct(state, state_code) %>%
  dplyr::rename(fips = state_code)

usethis::use_data(states, overwrite = TRUE)
