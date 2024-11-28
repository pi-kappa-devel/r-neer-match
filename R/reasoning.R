#' @include matching_model.R

## Classes (sorted lexicographically) ##########################################

#' @title Refutation Model
#' @description
#' Inherits \code{\link{NSMatchingModel}} and provides additional functionality
#' for refutation reasoning. The built-in refutation logic allows one to
#' refute the significance of one or more similarities of conjectured
#' association in detecting entity matches.
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
#' model <- RefutationModel(smap)
#' @export
RefutationModel <- function(similarity_map,
                            initial_feature_width_scales = 10L,
                            feature_depths = 2L,
                            initial_record_width_scale = 10L,
                            record_depth = 4L,
                            ...) {
  nm <- getOption("neermatch.py")
  nm$reasoning$RefutationModel(
    similarity_map,
    initial_feature_width_scales,
    feature_depths,
    initial_record_width_scale,
    record_depth,
    ...
  )
}
setOldClass("neer_match.reasoning.RefutationModel")

## Methods (sorted lexicographically) ##########################################

#' @rdname compile
#' @description
#' \subsection{\code{\link{RefutationModel}}}{
#' Refutation models, similar to neural-symbolic models (\code{NSMatchingModel}),
#' are fitted using a custom. The method sets the optimizer for model training.
#' }
#' @param optimizer A
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/optimizers}{
#' tf.keras.optimizers} optimizer object.
#' @export
setMethod(
  "compile", "neer_match.reasoning.RefutationModel",
  function(object,
           optimizer = tensorflow::tf$keras$optimizers$Adam()) {
    object$compile(optimizer = optimizer)
  }
)

#' @rdname evaluate
#' @description
#' \subsection{\code{\link{RefutationModel}}}{
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
  "evaluate", "neer_match.reasoning.RefutationModel",
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

#' @rdname fit
#' @description
#' \subsection{\code{\link{RefutationModel}}}{
#' The method constructs a data generator and an axiom generator from the
#' input data and uses the model's similarity map to fit the model while trying
#' to refute the refutation claim.
#'
#' In the default case of satisfiability weight equal to 1, the function
#' minimizes the satisfiability of the refutation claim
#' while penalizing the satisfiability of the matching axioms below the
#' penalty threshold. If the satisfiability weight is less than 1, the model is
#' trained to optimize the satisfiability of the refutation claim, while
#' penalizing a weighted sum of the satisfiability of the matching axioms and
#' the binary cross entropy loss for values below the penalty threshold.
#'
#' The penalty threshold sets tolerance for the matching axioms (and/or the
#' binary cross entropy loss) below which the penalty is applied. The penalty
#' scale sets the linear scale of the penalty when the threshold is not crossed.
#' The penalty decay sets the exponential decay of the penalty when the
#' threshold is crossed. The linear and exponential parts are combined using the
#' \href{https://www.tensorflow.org/api_docs/python/tf/keras/activations/elu}{
#' \code{tf.keras.activations.elu}} function.
#' }
#' @param epochs The number of epochs to train the model.
#' @param refutation The refutation claim. A single element named list, where
#' the name is a field pair association in the similarity map and the value
#' is a list of one or more similarities. If instead a string is provided, the
#' method uses all the similarities in the similarity map for the refutation.
#' @param penalty_threshold A numeric value in the range \eqn{[0, 1]} that
#' determines the threshold for the penalty. If the loss is below the threshold,
#' the penalty is applied.
#' @param penalty_scale A numeric value that determines the linear scale of the
#' penalty when the threshold is not crossed.
#' @param penalty_decay A numeric value in the range \eqn{[0, 1]} that
#' determines the exponential decay of the penalty when the threshold is
#' crossed.
#' @param satisfiability_weight A numeric value in the range \eqn{[0, 1]}
#' representing the weight allocated to the satisfiability loss of a
#' hybrid model.
#' @param verbose An integer indicating the verbosity level.
#' @param log_mod_n An positive integer that determines the frequency of
#' logging. The method logs every \code{log_mod_n} epochs.
#' @export
setMethod(
  "fit", "neer_match.reasoning.RefutationModel",
  function(object,
           left, right, matches,
           epochs,
           refutation,
           penalty_threshold = 0.95,
           penalty_scale = 1.0,
           penalty_decay = 0.1,
           satisfiability_weight = 1.0,
           verbose = 1L,
           log_mod_n = 1L,
           ...) {
    matching_model_fit(
      object,
      left, right, matches,
      epochs,
      refutation,
      penalty_threshold,
      penalty_scale,
      penalty_decay,
      satisfiability_weight,
      verbose,
      log_mod_n,
      ...
    )
  }
)

#' @rdname predict
#' @description
#' \subsection{\code{\link{RefutationModel}}}{
#' Extracts model predictions. It automatically constructs a data generator
#' from the left and right datasets iterating over all the elements of their
#' Cartesian product. The predictions are calculated using a custom loop
#' over the batches of the generator.
#' }
#' @param batch_size The batch size (integer).
#' @export
predict.neer_match.reasoning.RefutationModel <- function(
    object,
    left, right,
    batch_size = 32L,
    ...) {
  matching_model_predict(object, left, right, batch_size, ...)
}

#' @rdname suggest
#' @param batch_size The batch size (integer).
#' @export
setMethod(
  "suggest", "neer_match.reasoning.RefutationModel",
  function(object, left, right, count, batch_size = 32L, ...) {
    matching_model_suggest(object, left, right, count, batch_size, ...)
  }
)
