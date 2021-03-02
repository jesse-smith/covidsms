test_that("detect_java() works loudly", {
  test_loud_dialr <- detect_java(dialr =  TRUE, quiet = FALSE)
  test_loud       <- detect_java(dialr = FALSE, quiet = FALSE)
  expect_true(rlang::is_bool(test_loud_dialr))
  expect_true(rlang::is_bool(test_loud))
  expect_equal(test_loud_dialr, test_loud)
})

test_that("`detect_java()` works quietly", {
  test_quiet_dialr <- detect_java(dialr =  TRUE, quiet = TRUE)
  test_quiet       <- detect_java(dialr = FALSE, quiet = TRUE)
  expect_true(rlang::is_bool(test_quiet_dialr))
  expect_true(rlang::is_bool(test_quiet))
  expect_equal(test_quiet_dialr, test_quiet)
})

test_that("`detect_java()` value does not depend on `quiet`", {
  test_loud  <- detect_java(quiet = FALSE)
  test_quiet <- detect_java(quiet =  TRUE)
  expect_equal(test_loud, test_quiet)
})
