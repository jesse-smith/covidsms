test_that("`test_dialr()` works loudly", {
  test_loud <- test_dialr(quiet = FALSE)
  expect_true(rlang::is_bool(test_loud))
})

test_that("`test_dialr()` works quietly", {
  test_quiet <- test_dialr(quiet = TRUE)
  expect_true(rlang::is_bool(test_quiet))
})

test_that("`test_dialr()` value does not depend on `quiet`", {
  test_loud  <- test_dialr(quiet = FALSE)
  test_quiet <- test_dialr(quiet =  TRUE)
  expect_equal(test_loud, test_quiet)
})
