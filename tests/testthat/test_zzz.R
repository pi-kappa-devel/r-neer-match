test_that("The .onLoad function can be called", {
  expect_true(is.function(.onLoad))
  expect_no_error(.onLoad())
})
