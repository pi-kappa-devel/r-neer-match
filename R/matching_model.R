#' @importFrom reticulate r_to_py
#' @importFrom tensorflow tf
#' @include similarity_map.R

## Classes (sorted lexicographically) ##########################################

#' @title Deep Learning Matching Model
#' @description
#' Construct a deep learning entity-matching model based on field similarity
#' encodings. The class wraps the constructor of the \code{DLMatchingModel} Python
#' class from the \code{neer-match} package. The class is built using the
#' \code{\link{tensorflow}} framework.
#'
#' @param similarity_map A \code{\link{SimilarityMap}} object.
#' @param initial_feature_width_scales An integer or an integer vector of
#' initial feature width scales for each field-pair network. The scale is
#' multiplied by the number of similarities used in the field-pair network
#' to determine the number of units of the first dense layer. If the input is a
#' scalar, the same value is used for all field-pair networks.
#' @param feature_depths An integer or an integer vector of feature depths for
#' each field-pair network. The depth is the number of hidden dense layers
#' used in the field-pair network. If the input is a scalar, the same value is
#' used for all field-pair networks.
#' @param initial_record_width_scale An integer representing the initial record
#' width scale. The scale is multiplied by the number of field-pair networks
#' to determine the number of units of the first dense layer of the record-pair
#' network.
#' @param record_depth An integer representing the record depth. The depth is
#' the number of hidden dense layers used in the record-pair network.
#' @param ... Additional arguments passed to the Python constructor. These
#' arguments are passed down to the
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model}{
#' \code{tf.keras.Model}} constructor.
#' @seealso \code{\link{SimilarityMap}}, \code{\link{NSMatchingModel}}
#' @examples
#' smap <- SimilarityMap(
#'   instructions = list(
#'     `a~c` = list("discrete", "gaussian"),
#'     `b` = list("discrete", "levenshtein")
#'   )
#' )
#' model <- DLMatchingModel(smap)
#' @export
DLMatchingModel <- function(similarity_map,
                            initial_feature_width_scales = 10L,
                            feature_depths = 2L,
                            initial_record_width_scale = 10L,
                            record_depth = 4L,
                            ...) {
  # Parameter checking takes place in the Python constructor
  nm <- getOption("neermatch.py")
  nm$matching_model$DLMatchingModel(
    similarity_map,
    initial_feature_width_scales,
    feature_depths,
    initial_record_width_scale,
    record_depth,
    ...
  )
}
setOldClass("neer_match.matching_model.DLMatchingModel")

#' @title Neural-Symbolic Matching Model
#' @description
#' Construct a neural-symbolic learning entity-matching model based on field
#' similarity encodings. The class wraps the constructor of the
#' \code{NSMatchingModel} Python class from the \code{neer-match} package.
#' The class is built using the \code{\link{tensorflow}} and
#' \href{https://pypi.org/project/ltn/}{\code{ltn}} frameworks.
#'
#' The model uses the field-pair and record-pair networks of the
#' \code{\link{DLMatchingModel}} model as predicates. The fuzzy logic
#' component of the model aims to find weights for the predicates such
#' that for every matching example, at least one of the field-pair
#' predicates is (fuzzily) satisfied, and for every non-matching example,
#' not all of the field-pair predicates are (fuzzily) satisfied.
#'
#' In addition to purely neural-symbolic models, i.e., models trained
#' using only the satisfiability loss, the class can also be used to
#' construct hybrid models. Hybrid models are trained using a weighted
#' average of the satisfiability and the binary cross-entropy losses.
#'
#' @param similarity_map A \code{\link{SimilarityMap}} object.
#' @param initial_feature_width_scales An integer or an integer vector of
#' initial feature width scales for each field-pair network. The scale is
#' multiplied by the number of similarities used in the field-pair network
#' to determine the number of units of the first dense layer. If the input is a
#' scalar, the same value is used for all field-pair networks.
#' @param feature_depths An integer or an integer vector of feature depths for
#' each field-pair network. The depth is the number of hidden dense layers
#' used in the field-pair network. If the input is a scalar, the same value is
#' used for all field-pair networks.
#' @param initial_record_width_scale An integer representing the initial record
#' width scale. The scale is multiplied by the number of field-pair networks
#' to determine the number of units of the first dense layer of the record-pair
#' network.
#' @param record_depth An integer representing the record depth. The depth is
#' the number of hidden dense layers used in the record-pair network.
#' @param ... Additional arguments passed to the Python constructor. These
#' arguments are passed down to the
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model}{
#' \code{tf.keras.Model}} constructor.
#' constructor.
#' @seealso \code{\link{SimilarityMap}}
#' @examples
#' smap <- SimilarityMap(
#'   instructions = list(
#'     `z~w` = list("jaro", "levenshtein", "discrete"),
#'     `b ~ c` = list("jaro_winkler", "hamming")
#'   )
#' )
#' model <- NSMatchingModel(smap)
#' @export
NSMatchingModel <- function(similarity_map,
                            initial_feature_width_scales = 10L,
                            feature_depths = 2L,
                            initial_record_width_scale = 10L,
                            record_depth = 4L,
                            ...) {
  nm <- getOption("neermatch.py")
  nm$matching_model$NSMatchingModel(
    similarity_map,
    initial_feature_width_scales,
    feature_depths,
    initial_record_width_scale,
    record_depth,
    ...
  )
}
setOldClass("neer_match.matching_model.NSMatchingModel")

## Generics (sorted lexicographically) #########################################

#' @rdname compile
#' @title Model Compilation
#' @description
#' Compiles the model. The method prepares a neural network for estimation by
#' setting the optimizer, loss function, and other parameters as needed.
#' @param object A matching model object.
#' @param ... Additional arguments passed to
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model#compile}{
#' tf.keras.compile}.
#' @return Called for side effects.
#' @examples
#' smap <- SimilarityMap(
#'   instructions = list(
#'     `movie ~film` = list("jaro_winkler", "damerau_levenshtein"),
#'     `studio` = list("jaccard", "levenshtein"),
#'     `reviews~score` = list("euclidean")
#'   )
#' )
#' model <- DLMatchingModel(smap)
#' compile(model, optimizer = tensorflow::tf$keras$optimizers$Adam())
#' @export
setGeneric("compile", function(object, ...) {
  standardGeneric("compile")
})

#' @rdname evaluate
#' @title Model Evaluation
#' @description
#' Evaluates the model. Calculates and returns the loss and various accuracy
#' metrics for a fitted model and the passed data.
#'
#' The method emulates the behavior of the
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model#evaluate}{
#' tf.keras.evaluate} method. It automatically constructs a data generator
#' from the left and right datasets iterating over all the elements of their
#' Cartesian product. The generator's labels are generated from the matches
#' data frame. The method uses the generator to evaluate the model.
#'
#' @param object A matching model object.
#' @param left A data frame with the left records.
#' @param right A data frame with the right records.
#' @param matches A data frame with the indices of the matching record pairs.
#' @return Called for side effects (model evaluation).
#' @examples
#' smap <- SimilarityMap(
#'   instructions = list(
#'     `reviews` = list("discrete", "gaussian"),
#'     `developer ~ dev` = list("jaro_winkler", "damerau_levenshtein")
#'   )
#' )
#' model <- DLMatchingModel(smap)
#' compile(model, loss = tensorflow::tf$keras$losses$BinaryCrossentropy())
#'
#' matching_data <- fuzzy_games_example_data()
#'
#' eval_set <- c(1:3)
#' train_left <- matching_data$left[-eval_set, ]
#' train_right <- matching_data$right[-eval_set, ]
#' train_matches <- matching_data$matches[
#'   !(matching_data$matches$left %in% eval_set) |
#'     !(matching_data$matches$right %in% eval_set),
#' ] - 3L
#' fit(
#'   model,
#'   train_left, train_right, train_matches,
#'   epochs = 1L,
#'   verbose = 0L
#' )
#'
#' eval_left <- matching_data$left[eval_set, ]
#' eval_right <- matching_data$right[eval_set, ]
#' eval_matches <- matching_data$matches[
#'   (matching_data$matches$left %in% eval_set) &
#'     (matching_data$matches$right %in% eval_set),
#' ]
#' print(evaluate(model, eval_left, eval_right, eval_matches))
#' @export
setGeneric("evaluate", function(object, left, right, matches, ...) {
  standardGeneric("evaluate")
})

#' @rdname fit
#' @title Model Training
#' @description
#' Fits the model. The method emulates the behavior of the
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model#fit}{
#' tf.keras.fit} method. It automatically constructs a data generator
#' from the left and right datasets iterating over all the elements of their
#' Cartesian product. The generator's labels are generated from the matches
#' data frame. The method uses the generator to train the model.
#'
#' @param object A matching model object.
#' @param left A data frame with the left records.
#' @param right A data frame with the right records.
#' @param matches A data frame with the indices of the matching record pairs.
#' @return Called for side effects (model training).
#' @examples
#' smap <- SimilarityMap(
#'   instructions = list(
#'     `score` = list("gaussian", "euclidean"),
#'     `platform` = list("osa", "indel")
#'   )
#' )
#' model <- NSMatchingModel(smap)
#' compile(model)
#' matching_data <- fuzzy_games_example_data()
#' fit(
#'   model,
#'   matching_data$left, matching_data$right, matching_data$matches,
#'   epochs = 1L
#' )
#' @export
setGeneric("fit", function(object, left, right, matches, ...) {
  standardGeneric("fit")
})

#' @rdname suggest
#' @title Model Suggestions
#' @description
#' Suggests the most similar records. It automatically constructs a data
#' generator from the left and right datasets iterating over all the elements
#' of their Cartesian product. The method calls the
#' \code{\link[=predict.neer_match.matching_model.NSMatchingModel]{predict}}
#' method of the passed model using the constructed generator. For each record
#' in the left dataset of, the function returns \code{count} records from the
#' right dataset having the greatest matching predictions.
#'
#' @param object A matching model object.
#' @param left A data frame with the left records.
#' @param right A data frame with the right records.
#' @param count The number of returned suggestions from the right dataset
#' for each record in the left dataset.
#' @param ... Additional arguments passed to the
#' \code{\link[=predict.neer_match.matching_model.NSMatchingModel]{predict}}
#' method.
#' @return A data frame with the most similar records.
#' @examples
#' smap <- SimilarityMap(
#'   instructions = list(
#'     `score` = list("gaussian", "euclidean"),
#'     `platform` = list("osa", "indel")
#'   )
#' )
#' model <- NSMatchingModel(smap)
#' compile(model)
#' matching_data <- fuzzy_games_example_data()
#' fit(
#'   model,
#'   matching_data$left, matching_data$right, matching_data$matches,
#'   epochs = 1L,
#'   verbose = 0L
#' )
#' suggest(
#'   model, matching_data$left[1:2, ], matching_data$right[1:2, ],
#'   count = 2L
#' )
#' @export
setGeneric("suggest", function(object, left, right, count, ...) {
  standardGeneric("suggest")
})

## Methods (sorted lexicographically) ##########################################

#' @rdname compile
#' @description
#' \subsection{\code{\link{DLMatchingModel}}}{
#' The method calls the
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model#compile}{
#' tf.keras.compile} method of the \code{tf.keras.Model} class.
#' }
#' @export
setMethod(
  "compile", "neer_match.matching_model.DLMatchingModel",
  function(object, ...) {
    tensorflow::tf$keras$Model$compile(object, ...)
  }
)

#' @rdname compile
#' @description
#' \subsection{\code{\link{NSMatchingModel}}}{
#' Neural symbolic models are fit using a custom training loop so there is
#' no need to calls
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model#compile}{
#' tf.keras.compile} method of the \code{tf.keras.Model} class. The method
#' sets the optimizer for the model. The loss (for hybrid neural-symbolic and
#' deep learning models) is set to binary cross-entropy (
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/losses/BinaryCrossentropy}{
#' tf.keras.losses.BinaryCrossentropy})
#' }
#' @param optimizer A
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/optimizers}{
#' tf.keras.optimizers} optimizer object.
#' @export
setMethod(
  "compile", "neer_match.matching_model.NSMatchingModel",
  function(object,
           optimizer = tensorflow::tf$keras$optimizers$Adam()) {
    object$compile(optimizer = optimizer)
  }
)

matching_model_evaluate <- function(object, left, right, ...) {
  py_left <- reticulate::r_to_py(left)
  py_right <- reticulate::r_to_py(right)
  object$evaluate(py_left, py_right, ...)
}

#' @rdname evaluate
#' @description
#' \subsection{\code{\link{DLMatchingModel}}}{
#' The method passes the constructed generator and any
#' additional call arguments to
#' directly to the
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model#evaluate}{
#' tf.keras.evaluate}.
#' }
#' @param ... Additional arguments passed to
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model#evaluate}{
#' tf.keras.evaluate}.
#' @export
setMethod(
  "evaluate", "neer_match.matching_model.DLMatchingModel",
  function(object, left, right, matches, ...) {
    matching_model_evaluate(object, left, right, matches, ...)
  }
)

#' @rdname evaluate
#' @description
#' \subsection{\code{\link{NSMatchingModel}}}{
#' The method iterates over the batches of the constructed generator and
#' calculates the numbers of true positives, true negatives, false positives,
#' and false negatives. In addition, it calculates accuracy, precision, recall,
#' and F1 score. The method returns a named list with the calculated metrics.
#' }
#' @param batch_size The batch size (integer).
#' @param satisfiability_weight A numeric value in the range \eqn{[0, 1]}
#' representing the weight of the satisfiability loss in the total loss of a
#' hybrid model.
#' @export
setMethod(
  "evaluate", "neer_match.matching_model.NSMatchingModel",
  function(object,
           left, right, matches,
           batch_size = 32L,
           satisfiability_weight = 1.0) {
    matching_model_evaluate(
      object,
      left, right, matches,
      batch_size, satisfiability_weight
    )
  }
)

matching_model_fit <- function(object, left, right, matches, ...) {
  py_left <- reticulate::r_to_py(left)
  py_right <- reticulate::r_to_py(right)
  py_matches <- reticulate::r_to_py(matches) - 1L
  object$fit(py_left, py_right, py_matches, ...)
}

#' @rdname fit
#' @description
#' \subsection{\code{\link{DLMatchingModel}}}{
#' The method passes the constructed generator and any
#' additional call arguments to
#' directly to the
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model#fit}{
#' tf.keras.fit}.
#' }
#' @param ... Additional arguments passed to
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model#fit}{
#' tf.keras.fit}.
#' @export
setMethod(
  "fit", "neer_match.matching_model.DLMatchingModel",
  function(object, left, right, matches, ...) {
    matching_model_fit(object, left, right, matches, ...)
  }
)

#' @rdname fit
#' @description
#' \subsection{\code{\link{DLMatchingModel}}}{
#' The method passes the constructed generator and any
#' additional call arguments to
#' directly to the
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model#fit}{
#' tf.keras.fit}.
#' }
#' @param epochs The number of epochs to train the model.
#' @param satisfiability_weight A numeric value in the range \eqn{[0, 1]}
#' representing the weight allocated to the satisfiability loss of a
#' hybrid model.
#' @param verbose An integer indicating the verbosity level.
#' @param log_mod_n An positive integer that determines the frequency of
#' logging. The method logs every \code{log_mod_n} epochs.
#' @export
setMethod(
  "fit", "neer_match.matching_model.NSMatchingModel",
  function(object,
           left, right, matches,
           epochs,
           satisfiability_weight = 1.0,
           verbose = 1L,
           log_mod_n = 1L,
           ...) {
    matching_model_fit(
      object,
      left, right, matches,
      epochs,
      satisfiability_weight,
      verbose,
      log_mod_n,
      ...
    )
  }
)

matching_model_predict <- function(object, left, right, ...) {
  py_left <- reticulate::r_to_py(left)
  py_right <- reticulate::r_to_py(right)
  preds <- object$predict(py_left, py_right, ...)
  as.vector(preds)
}

#' @rdname predict
#' @title Model Predictions
#' @description
#' \subsection{\code{\link{DLMatchingModel}}}{
#' Extracts model predictions. The method calls the
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model#predict}{
#' tf.keras.predict} method of the. It automatically constructs a data generator
#' from the left and right datasets iterating over all the elements of their
#' Cartesian product.
#' }
#' @param object A matching model object.
#' @param left A data frame with the left records.
#' @param right A data frame with the right records.
#' @param ... Additional arguments passed to the
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/Model#predict}{
#' tf.keras.predict} method.
#' @return The model predictions.
#' @examples
#' smap <- SimilarityMap(
#'   instructions = list(
#'     `score` = list("gaussian", "euclidean"),
#'     `platform` = list("osa", "indel")
#'   )
#' )
#' model <- DLMatchingModel(smap)
#' compile(model, loss = tensorflow::tf$keras$losses$BinaryCrossentropy())
#' matching_data <- fuzzy_games_example_data()
#' fit(
#'   model,
#'   matching_data$left, matching_data$right, matching_data$matches,
#'   epochs = 1L,
#'   verbose = 0L
#' )
#' predict(model, matching_data$left[1:2, ], matching_data$right[1:2, ])
#' @export
predict.neer_match.matching_model.DLMatchingModel <- function(
    object,
    left,
    right, ...) {
  matching_model_predict(object, left, right, ...)
}

#' @rdname predict
#' @description
#' \subsection{\code{\link{NSMatchingModel}}}{
#' Extracts model predictions. It automatically constructs a data generator
#' from the left and right datasets iterating over all the elements of their
#' Cartesian product. The predictions are calculated using a custom loop
#' over the batches of the generator.
#' }
#' @param batch_size The batch size (integer).
#' @export
predict.neer_match.matching_model.NSMatchingModel <- function(
    object,
    left, right,
    batch_size = 32L,
    ...) {
  matching_model_predict(object, left, right, batch_size, ...)
}

matching_model_suggest <- function(object, left, right, ...) {
  py_left <- reticulate::r_to_py(left)
  py_right <- reticulate::r_to_py(right)
  object$suggest(py_left, py_right, ...)
}

#' @rdname suggest
#' @export
setMethod(
  "suggest", "neer_match.matching_model.DLMatchingModel",
  function(object, left, right, count, ...) {
    matching_model_suggest(object, left, right, count, ...)
  }
)

#' @rdname suggest
#' @param batch_size The batch size (integer).
#' @export
setMethod(
  "suggest", "neer_match.matching_model.NSMatchingModel",
  function(object, left, right, count, batch_size = 32L, ...) {
    matching_model_suggest(object, left, right, count, batch_size, ...)
  }
)
