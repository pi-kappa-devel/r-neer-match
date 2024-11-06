#' @include zzz.R

#' @title Game Reviews Data
#'
#' @details
#' \subsection{Data Description.}{
#' The data is a copy of an archived version of the game ranking data of
#' GameRankings.com before it shut down. The snapshot was taken 2019-12-08.
#' The variables are as follows:
#'
#' \itemize{
#'   \item \code{title (character)} The game title.
#'   \item \code{platform (character)} The release platform of the game.
#'   \item \code{year (integer)} The year the game was released.
#'   \item \code{score (numeric)} The average user review score of the game.
#'   \item \code{reviews (integer)} The number of user reviews.
#'   \item \code{developer (character)} The developer of the game.
#' }
#' }
#'
#' @source
#' \href{https://docs.google.com/spreadsheets/d/1wtV8yBr5RXAjO_1kakzFcHd7TKGw7pIUVNI5SYouCUM}{
#' GameRankings.com data dump on 2019-12-08}
#'
#' @examples
#' data(game_reviews)
#' head(game_reviews)
#' @docType data
#' @keywords datasets games reviews
#' @name game_reviews
#' @usage data(game_reviews)
#' @format A data frame with 47201 rows and 6 columns
NULL

#' @title Fuzzy Games Matching Example Data
#' @description Returns three datasets used in the
#' \code{vignette("fuzzy_games", package = "neermatch")} vignette.
#'
#' The left dataset is a subset of the `game_reviews` data containing games
#' with titles starting with "Metal Gear" or "Metal Slug". The selection
#' results in 36 records, many of which have the exact same `title`,
#' `developer`, and release `year` and differ only in their release
#' `platform`, `score`, and `reviews` fields.
#'
#' The initial selection of the right records is the same as in the left,
#' but the data of the right dataset introduce noise in the `title` and
#' `developer` columns. Three characters are randomly removed from the
#' `title` column, and one character is randomly removed from the
#' `developer` column. The mutated `developer` column is stored in the
#' column `dev` of the right dataset. In addition, three records of the
#' left dataset are randomly duplicated in the right dataset.
#'
#' The matches dataset is a data frame with the matching indices of the
#' left and right datasets.
#'
#' @returns A left data frame with 36 rows and 6 columns. A right data frame
#' with 39 rows and 6 columns. A matches data frame with 39 rows and 2 columns.
#' @examples
#' matching_data <- fuzzy_games_example_data()
#' head(matching_data$left)
#' head(matching_data$right)
#' head(matching_data$matches)
#' @seealso \code{\link{game_reviews}}
#' @keywords datasets games reviews
#' @export
fuzzy_games_example_data <- function() {
  left <- neermatch::game_reviews[
    startsWith(neermatch::game_reviews$title, c("Metal Gear", "Metal Slug")),
    c("title", "platform", "year", "score", "reviews", "developer")
  ]

  right <- left
  duplicates <- sample(seq.int(nrow(right)), 3)
  right <- rbind(right, right[duplicates, ])
  right$title <- sapply(right$title, function(x) {
    paste0(unlist(strsplit(x, ""))[-sample(1:nchar(x), 3)], collapse = "")
  })
  right$dev <- sapply(right$developer, function(x) {
    paste0(unlist(strsplit(x, ""))[-sample(1:nchar(x), 1)], collapse = "")
  })
  right <- right[, c("title", "platform", "year", "score", "reviews", "dev")]

  matches <- data.frame(
    left = c(seq_len(nrow(left)), duplicates),
    right = seq_len(nrow(right))
  )

  list(left = left, right = right, matches = matches)
}
