# Get FIPS codes for states from the `tidycensus` package

# install.packages("tidycensus")

states <- tidycensus::fips_codes %>%
  dplyr::distinct(state, state_code) %>%
  dplyr::rename(fips = state_code) %>%
  dplyr::as_tibble()

usethis::use_data(states, overwrite = TRUE)
