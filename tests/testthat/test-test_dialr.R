test_that("`test_dialr()` works", {
  test_loud <- test_dialr()
  test_quiet <- test_dialr(quiet = TRUE)
  expect_true(rlang::is_bool(test_loud))
  expect_true(rlang::is_bool(test_quiet))
  expect_equal(test_loud, test_quiet)
})
