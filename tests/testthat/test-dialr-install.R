test_that("dialr is installed correctly", {
  skip_if_not_installed("dialr")
  expect_true(test_dialr())
})
