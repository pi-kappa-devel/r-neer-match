#' @include zzz.R

#' @title Similarity Map
#' @description
#' A similarity map is a list of instructions that define how to compare two
#' data sets. Each instruction defines a pair of associated fields in the
#' two data sets and one or more similarity functions that should be used to
#' compare the fields.
#' @details
#' The similarity map is used to create a similarity encoder. Similarity
#' encoders are used (1) to encode pairs of data sets into a single tensor
#' that can be used as input to a neural network, and (2) to retrieve
#' information about constructing refutation claims for reasoning with
#' neural-symbolic matching models. No actual encoding operation are performed
#' by the similarity map. This design allows defining the similarity map once
#' and using it to encode multiple pairs of data sets or to encode different
#' subsets of the same data.
#'
#' @section Instructions:
#' Instructions are named lists of vectors. Each instruction (list element)
#' defines
#' \itemize{
#' \item{a pair of fields, one from each data set, that should be compared
#' (element key) and}
#' \item{a collection of similarity functions that should be used to compare
#' the associated fields (element value).}
#' }
#' The instruction keys should follow the association string format.
#'
#' @section Association Strings:
#' An association string is a string that contains two field names separated by
#' a tilde (~) character. The first field name is the name of the field in the
#' left data set and the second field name is the name of the field in the
#' right data set. In the general case, where the field names are different
#' in the two data sets, the association string should be formatted as
#' `left_column~right_column`. This instructs the model to associate the
#' `left_column` of the `Left` input data with the `right_column` of the `Right`
#' input data.
#'
#' If the column names of the input data for a field are identical for both
#' records, the association string can be the common field name. For example,
#' if both the `Left` and `Right` input dataset have a column named `title`,
#' then an association string `title` instructs the model to associate the
#' `title` column of the `Left` and `Right` input data.
#'
#' @section Similarity Functions:
#' The vector of similarity functions defines the operations of the similarity
#' encoder for each association string. Each association can have multiple
#' similarity operations, in which case the similarity encoder applies all
#' similarity operations to the associated columns. The caller can define the
#' similarity operations by providing one of the predefined similarities.
#' The predefined string similarities and ratios are calculated using the
#' implementation of
#' [RapidFuzz](https://maxbachmann.github.io/RapidFuzz/).
#' A list of  available similarities can be obtained by calling
#' \code{\link{available_similarities}}.
#'
#' @param instructions A list of instructions that define how to compare
#' @return A similarity map object
#' @examples
#' instructions <- list(
#'   "title" = c("damerau_levenshtein", "jaro"),
#'   "author~author" = c("lcsseq", "hamming"),
#'   "year~year" = c("gaussian", "euclidean")
#' )
#' smap <- SimilarityMap(instructions)
#' @seealso \code{\link{available_similarities}}
#' @export
SimilarityMap <- function(instructions) {
  nm <- getOption("neermatch.py")
  nm$similarity_map$SimilarityMap(instructions)
}
setOldClass("neer_match.similarity_map.SimilarityMap")

## Generics (Lexicographically sorted) #########################################

#' @rdname association_names
#' @export
setGeneric("association_names", function(object) {
  standardGeneric("association_names")
})

#' @rdname association_offsets
#' @export
setGeneric("association_offsets", function(object) {
  standardGeneric("association_offsets")
})

#' @rdname association_sizes
#' @export
setGeneric("association_sizes", function(object) {
  standardGeneric("association_sizes")
})

#' @rdname no_associations
#' @export
setGeneric("no_associations", function(object) {
  standardGeneric("no_associations")
})

#' @rdname similarity_keys
#' @export
setGeneric("similarity_keys", function(object) {
  standardGeneric("similarity_keys")
})

## Methods (Lexicographically sorted) ##########################################

#' @rdname as.list
#' @title Convert a similarity map to a list
#' @description
#' Returns a nested parsed list of instructions that define how to compare two
#' data sets. Each instruction (outer list element) has three parts (inner
#' list). The first part is the left field name, the second part is the right
#' field, and the third part is a similarity function.
#' @param x A similarity map object.
#' @param ... Additional arguments (not used).
#' @return A list of instructions.
#' @examples
#' smap <- SimilarityMap(list(
#'   "title" = list("jaro"),
#'   "author~who" = list("lcsseq", "hamming")
#' ))
#' as.list(smap)
#' @export
as.list.neer_match.similarity_map.SimilarityMap <- function(x, ...) {
  lapply(seq_along(x), function(i) {
    x[[i - 1]]
  })
}

#' @rdname available_similarities
#' @title Predefined available similarities
#' @return A list of predefined available similarities.
#' @examples
#' available_similarities()
#' @export
available_similarities <- function() {
  nm <- getOption("neermatch.py")
  sims <- nm$similarity_map$available_similarities()
  reticulate::py_to_r(sims)
}

#' @rdname association_names
#' @title Association names
#' @description
#' A list with field pair names separated by underscores.
#' @param object A similarity map object.
#' @return A list of association names.
#' @examples
#' smap <- SimilarityMap(list(
#'   "title" = list("jaro"),
#'   "author~who" = list("lcsseq", "hamming")
#' ))
#' association_names(smap)
#' @export
setMethod("association_names", "neer_match.similarity_map.SimilarityMap", function(object) {
  object$association_names()
})

#' @rdname association_offsets
#' @title Association offsets
#' @description
#' A list with similarity matrix offsets for each field pair.
#' @param object A similarity map object.
#' @return A list of offsets.
#' @examples
#' smap <- SimilarityMap(list(
#'   "title" = list("jaro"),
#'   "author~who" = list("lcsseq", "hamming")
#' ))
#' association_offsets(smap)
#' @export
setMethod("association_offsets", "neer_match.similarity_map.SimilarityMap", function(object) {
  object$association_offsets() + 1L
})

#' @rdname association_sizes
#' @title Association sizes
#' @description
#' A list with the number of similarity functions for each field pair.
#' @param object A similarity map object.
#' @return A list of sizes.
#' @examples
#' smap <- SimilarityMap(list(
#'   "title" = list("jaro"),
#'   "author~who" = list("lcsseq", "hamming")
#' ))
#' association_sizes(smap)
#' @export
setMethod("association_sizes", "neer_match.similarity_map.SimilarityMap", function(object) {
  object$association_sizes()
})

#' @rdname no_associations
#' @title Number of associations
#' @description
#' The number of associations in the similarity map.
#' @param object A similarity map object.
#' @return The number of associations.
#' @examples
#' smap <- SimilarityMap(list(
#'   "title" = list("jaro"),
#'   "author~who" = list("lcsseq", "hamming")
#' ))
#' no_associations(smap)
#' @export
setMethod("no_associations", "neer_match.similarity_map.SimilarityMap", function(object) {
  object$no_associations()
})

#' @rdname similarity_keys
#' @title Similarity keys
#' @description
#' A list combining association names and similarity functions seperated by
#' underscores.
#' @param object A similarity map object.
#' @return A list of keys.
#' @examples
#' smap <- SimilarityMap(list(
#'   "title" = list("jaro"),
#'   "author~who" = list("lcsseq", "hamming")
#' ))
#' similarity_keys(smap)
#' @export
setMethod("similarity_keys", "neer_match.similarity_map.SimilarityMap", function(object) {
  object$keys()
})

#' @rdname show
#' @title Print the similarity map to standard output
#' @param object A similarity map object.
#' @return Called for its side effect (print to standard output).
#' @examples
#' smap <- SimilarityMap(list(
#'   "title" = list("damerau_levenshtein", "jaro"),
#'   "author~who" = list("lcsseq", "hamming"),
#'   "when~date" = list("euclidean")
#' ))
#' show(smap)
#' @export
setMethod("show", "neer_match.similarity_map.SimilarityMap", function(object) {
  cat(object$`__str__`(), "\n")
  invisible(object)
})
