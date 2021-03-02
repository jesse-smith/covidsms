test_that("`std_phone()` works with defaults", {
  expect_equal(
    std_phone(c("9012222000", "000")),
    c("9012222000", NA_character_)
  )
  expect_equal(std_phone(c(9012222000, 000)), c("9012222000", NA_character_))
})

test_that("`std_phone()` works without dialr", {
  expect_equal(
    std_phone(c("9012222000", "000", "1111111111"), dialr = FALSE),
    c("9012222000", NA_character_, "1111111111")
  )
  expect_equal(
    std_phone(c(9012222000, 000, 1111111111), dialr = FALSE),
    c("9012222000", NA_character_, "1111111111")
  )
})

test_that("`std_phone()` works with dialr", {
  skip_if_not(test_dialr(quiet = TRUE), "dialr is not available")
  expect_equal(
    std_phone(c("9012222000", "000", "1111111111"), dialr = TRUE),
    c("9012222000", NA_character_, NA_character_)
  )
  expect_equal(
    std_phone(c(9012222000, 000, 1111111111), dialr = TRUE),
    c("9012222000", NA_character_, NA_character_)
  )
})
