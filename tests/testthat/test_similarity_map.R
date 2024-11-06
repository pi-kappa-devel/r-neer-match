left_col <- sample(4:10, 1)
right_col <- sample(4:10, 1)
common_col <- min(left_col, right_col)

left_names <- lapply(seq_len(left_col), function(i) {
  paste0(sample(LETTERS, sample(5:20, 1)), collapse = "")
})
right_names <- lapply(seq_len(right_col), function(i) {
  paste0(sample(letters, sample(5:20, 1)), collapse = "")
})

associations <- paste0(left_names[1:common_col], "~", right_names[1:common_col])

similarities <- names(available_similarities())

instructions <- lapply(seq_len(common_col), function(i) {
  as.list(similarities[sample(length(similarities), sample(1:3, 1))])
})

names(instructions) <- associations

smap <- SimilarityMap(instructions = instructions)

test_that("No of pairs in similarity map can be calculated", {
  expect_equal(no_associations(smap), length(instructions))
})


test_that("No of similarities in similarity map can be calculated", {
  expect_equal(length(smap), sum(sapply(instructions, length)))
})

test_that("Pair field names can be extracted", {
  input_names <- association_names(smap)
  expected <- gsub("~", "_", names(instructions))
  expect_equal(input_names, expected)
})

test_that("Pair offsets can be extracted", {
  offsets <- association_offsets(smap)
  sizes <- association_sizes(smap)
  names(offsets) <- NULL
  expect_equal(length(offsets), no_associations(smap))
  expect_equal(offsets[1], 1L)
  expect_equal(
    offsets[length(offsets)] + sizes[length(sizes)] - 1,
    length(smap)
  )
})

test_that("Pair sizes can be extracted", {
  sizes <- association_sizes(smap)
  expect_equal(length(sizes), no_associations(smap))
  expect_equal(sum(sizes), length(smap))
})

test_that("Similarity instructions can be extracted", {
  expect_equal(length(similarity_keys(smap)), sum(sapply(instructions, length)))
})

test_that("Similarity map can be printed", {
  expect_output(show(smap), "SimilarityMap")
})

test_that("Similarity map can be converted to list", {
  slist <- as.list(smap)
  expect_type(slist, "list")
  expect_equal(length(slist), length(smap))
})
