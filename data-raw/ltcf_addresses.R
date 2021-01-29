# This data set is only available from the Shelby County COVID-19 Drive, via
# `ltcf_addr_path`

library(postmastr)

# Read data and prep for parsing -----------------------------------------------
addr <- readxl::read_excel(
  "V:/EPI DATA ANALYTICS TEAM/Case Assignment/LTCF Addresses.xlsx",
  col_types = "text"
) %>%
  dplyr::transmute(
    name,
    address = stringr::str_glue("{address} {city} {state} {postal}") %>%
      as.character() %>%
      stringr::str_to_upper()
  ) %>%
  pm_identify(var = "address")

# Create dictionaries ----------------------------------------------------------
state_dict <- pm_dictionary(type = "state", filter = "TN", case = "upper")

city_dict <- pm_append(
  type = "city",
  input = c(
    "ARLINGTON",
    "BARTLETT",
    "COLLIERVILLE",
    "CORDOVA",
    "GERMANTOWN",
    "MEMPHIS",
    "MILLINGTON"
  )
)

directional_dict <- pm_dictionary(type = "directional", case = "upper")

suffix_dict <- pm_dictionary(
  type = "suffix",
  append = pm_append(
    type = "suffix",
    input = c("COMMON", "CMN"),
    output = rep("Cmn", times = 2L)
  ),
  case = "upper"
)

# Parse addresses --------------------------------------------------------------
ltcf_addresses <- addr %>%
  pm_prep(var = "address", type = "street") %>%
  pm_postal_parse() %>%
  pm_state_parse(dictionary = state_dict) %>%
  pm_city_parse(dictionary = city_dict) %>%
  pm_house_parse() %>%
  pm_streetDir_parse(dictionary = directional_dict) %>%
  pm_streetSuf_parse(dictionary = suffix_dict) %>%
  pm_street_parse() %>%
  dplyr::mutate(dplyr::across(where(is.character), stringr::str_to_upper)) %>%
  pm_replace(source = addr) %>%
  pm_rebuild(output = "full", keep_parsed = "yes") %>%
  dplyr::select(-address)

# Save data --------------------------------------------------------------------
fst::write_fst(
  ltcf_addresses,
  path = "V:/EPI DATA ANALYTICS TEAM/Case Assignment/ltcf_addresses.fst",
  compress = 100L
)
