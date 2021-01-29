# This dataset is only available on the Shelby County COVID-19 Drive, via
# the `ltcf_phones_path` variable
p <- readxl::read_excel(
  "V:/EPI DATA ANALYTICS TEAM/Case Assignment/LTCF Phones.xlsx"
)

p %>%
  dplyr::transmute(phone = std_phone(contact_number)) %>%
  dplyr::arrange(phone) %>%
  fst::write_fst(path = ltcf_phone_path, compress = 100L)
