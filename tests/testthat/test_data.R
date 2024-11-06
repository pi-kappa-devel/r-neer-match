test_that("Fuzzy games example data can be loaded", {
  matching_data <- fuzzy_games_example_data()
  for (ds in matching_data) {
    expect_true(is.data.frame(ds))
  }
  expect_equal(nrow(matching_data$left), 36)
  expect_equal(nrow(matching_data$right), 39)
  expect_equal(ncol(matching_data$left), ncol(matching_data$right))
  expect_equal(ncol(matching_data$right), 6)
  expect_equal(
    names(matching_data$left),
    c("title", "platform", "year", "score", "reviews", "developer")
  )
})
