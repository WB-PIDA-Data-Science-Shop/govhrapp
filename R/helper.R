#' Guess the Reporting Frequency of the Reference Dates
#'
#' Evaluates a vector of reference dates and returns a single
#' string representing the data's reporting interval (e.g., "year", "month").
#' The function calculates the median day difference between consecutive dates.
#'
#' @param ref_date A vector of dates (inheriting from \code{Date} or \code{POSIXt}).
#'
#' @return A single character scalar: \code{"year"}, \code{"quarter"}, 
#'   \code{"month"}, \code{"week"}, or \code{"day"}.
#'
#' @export
#'
#' @examples
#' # Monthly reporting dates
#' dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
#' guess_date_frequency(dates)
#' #> [1] "month"
guess_date_frequency <- function(.data) {
  ref_date <- .data[["ref_date"]] |> 
    unique() |> 
    sort()

  median_days <- median(diff(as.Date(ref_date)), na.rm = TRUE)
  
  if (median_days >= 360) return("year")
  if (median_days >= 80)  return("quarter")
  if (median_days >= 27)  return("month")
  if (median_days >= 6)   return("week")
  return("day")
}
