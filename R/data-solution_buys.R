#' Solution file of sessions that bought at least one item.
#'
#' A \code{data.frame} object of purchase data.
#'
#' \describe{
#'     \item{Session_ID}{A sequence of integers, represented as a string, used to uniquely identify a session.}
#'     \item{ind_buy}{A factor indicating that the session bought at least one item.}
#'     \item{Items}{A comma-delimited string indicating the item(s) bought}
#' }
#' @docType data
#' @name solution_buys
#' @usage solution_buys
#' @format A \code{data.frame} object of three columns and approximately 128,000 rows.
#' @examples
#' solution_buys
NULL
