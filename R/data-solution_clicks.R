#' The test data set of clickstream data.
#'
#' A \code{data.frame} object of clickstream data, specifically product views.
#'
#' \describe{
#'     \item{Session_ID}{A sequence of integers, represented as a string, used to uniquely identify a session.}
#'     \item{Timestamp}{A timestamp of YYYY-MM-DD hh:mm:ss format indicating when a product was viewed.}
#'     \item{Item_ID}{A sequence of integers, represented as a string, used to uniquely identify a product.}
#'     \item{Category}{A string used to identify the corresponding category of an item.}
#' }
#' @docType data
#' @name solution_clicks
#' @usage solution_clicks
#' @format A \code{data.frame} object of four columns and approximately 8MM rows.
#' @examples
#' solution_clicks
NULL
