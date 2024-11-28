smap <- SimilarityMap(
  instructions = list(
    `review~score` = list("gaussian", "discrete"),
    title = list("levenshtein", "jaro_winkler")
  )
)

left <- data.frame(
  review = c(9, 8.5, 7),
  title = c("Title A", "Title B", "Title C")
)

right <- data.frame(
  score = c(7, 10, 9),
  title = c("B Title", "A Title", "C Title")
)

matches <- data.frame(
  left = as.integer(c(1, 2, 3)),
  right = as.integer(c(2, 1, 3))
)

dl_model <- DLMatchingModel(smap)
ns_model <- NSMatchingModel(smap)
rr_model <- RefutationModel(smap)

test_that("Matching models can be compiled", {
  expect_no_error(compile(
    dl_model,
    loss = tensorflow::tf$keras$losses$BinaryCrossentropy()
  ))
  expect_no_error(compile(ns_model))
  expect_no_error(compile(rr_model))
})

test_that("Matching models can be fitted", {
  expect_no_error(fit(
    dl_model, left, right, matches,
    epochs = 1L, verbose = 0L
  ))
  expect_no_error(fit(
    ns_model, left, right, matches,
    epochs = 1L, verbose = 0L
  ))
  expect_no_error(fit(
    rr_model, left, right, matches,
    refutation = "title",
    epochs = 1L, verbose = 0L
  ))
})

test_that("Matching models can be evaluated", {
  dl_eval <- evaluate(dl_model, left, right, matches, verbose = 0L)
  ns_eval <- evaluate(ns_model, left, right, matches)
  rr_eval <- evaluate(rr_model, left, right, matches)
  expect_type(dl_eval, "double")
  expect_type(ns_eval, "list")
  expect_type(rr_eval, "list")
  expect_length(ns_eval, 11)
  expect_length(rr_eval, 11)
})

test_that("Matching models can produce predictions", {
  dl_pred <- predict(dl_model, left, right, verbose = 0L)
  ns_pred <- predict(ns_model, left, right)
  rr_pred <- predict(rr_model, left, right)
  expect_type(dl_pred, "double")
  expect_type(ns_pred, "double")
  expect_type(rr_pred, "double")
  expect_equal(length(dl_pred), length(ns_pred))
  expect_equal(length(dl_pred), 9)
  expect_equal(length(rr_pred), 9)
})

test_that("Matching models can produce suggestions", {
  dl_sugg <- suggest(dl_model, left, right, verbose = 0L, count = 2L)
  ns_sugg <- suggest(ns_model, left, right, count = 2L)
  rr_sugg <- suggest(rr_model, left, right, count = 2L)
  expect_true(is(dl_sugg, "data.frame"))
  expect_true(is(ns_sugg, "data.frame"))
  expect_true(is(rr_sugg, "data.frame"))
  expect_equal(dim(dl_sugg), dim(ns_sugg))
  expect_equal(dim(dl_sugg), c(6, 3))
  expect_equal(dim(rr_sugg), c(6, 3))
})
