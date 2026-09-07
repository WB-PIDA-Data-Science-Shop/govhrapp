#' Guess the Reporting Frequency of the Reference Dates
#'
#' Infers the reporting interval of a panel from the median gap between
#' consecutive distinct reference dates.
#'
#' @param .data Data frame containing a `ref_date` column.
#'
#' @return A character scalar: `"year"`, `"quarter"`, `"month"`, `"week"` or
#'   `"day"`.
#'
#' @examples
#' # Monthly reporting dates
#' data <- data.frame(
#'   ref_date = seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "months")
#' )
#'
#' guess_date_frequency(data)
#' #> [1] "month"
#'
#' @importFrom stats median
#' @export
guess_date_frequency <- function(.data) {
  ref_date <- .data[["ref_date"]] |>
    unique() |>
    sort()

  median_days <- stats::median(diff(as.Date(ref_date)), na.rm = TRUE)

  thresholds <- c(year = 360, quarter = 80, month = 27, week = 6)
  matched <- names(thresholds)[median_days >= thresholds]

  if (length(matched) == 0) "day" else matched[1]
}

#' Render a Workforce Movement Value Box
#'
#' Renders the count and rate of one movement type at the latest reference
#' period. Values are read from the analytics cache when available and computed
#' from `.data` otherwise.
#'
#' @param .data Data frame containing personnel data.
#' @param movement_type Character. One of `"hire"`, `"fire"`, `"retirement"` or
#'   `"turnover"`.
#' @param cache List of pre-computed summaries from [build_analytics_cache()],
#'   or `NULL` to compute the values directly.
#'
#' @return A Shiny render function producing the value box.
#'
#' @importFrom bsicons bs_icon
#' @importFrom bslib popover value_box value_box_theme
#' @importFrom purrr pluck
#' @importFrom shiny h5 renderUI tagList
#' @importFrom stringr str_to_title
#' @keywords internal
render_movement_box <- function(.data, movement_type, cache = NULL) {
  values <- purrr::pluck(cache, "workforce", "movement_box", movement_type)

  if (is.null(values)) {
    values <- summarise_movement_box(.data, movement_type)
  }

  box_value <- if (movement_type == "turnover") {
    shiny::tagList(
      shiny::h5("Ratio of Hires"),
      shiny::h5(paste("to Exits:", round(values[["rate"]], 3)))
    )
  } else {
    shiny::tagList(
      shiny::h5(paste("Count:", values[["count"]])),
      shiny::h5(paste("Rate:", round(values[["rate"]], 3), "%"))
    )
  }

  shiny::renderUI({
    bslib::value_box(
      title = paste0(
        stringr::str_to_title(movement_type),
        " (", format(values[["ref_date"]], "%b %Y"), ")"
      ),
      theme = bslib::value_box_theme(bg = "#C34729", fg = "#ffffff"),
      class = "border",
      max_height = "150px",
      value = box_value,
      showcase = bsicons::bs_icon(
        switch(
          movement_type,
          hire = "person-plus-fill",
          fire = "person-dash-fill",
          retirement = "person-badge-fill",
          turnover = "arrow-repeat"
        )
      ),
      bslib::popover(
        bsicons::bs_icon("info-circle-fill"),
        placement = "left",
        switch(
          movement_type,
          hire = "Number and share of personnel hired in the most recent reference period.",
          fire = "Number and share of personnel separated (voluntary and involuntary) in the most recent reference period.",
          retirement = "Number and share of personnel retired in the most recent reference period.",
          turnover = "Ratio of hires to separations (including retirements) in the most recent reference period."
        )
      )
    )
  })
}
