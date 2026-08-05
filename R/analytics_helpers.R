#' Guess the Reporting Frequency of the Reference Dates
#'
#' Evaluates a vector of reference dates and returns a single
#' string representing the data's reporting interval (e.g., "year", "month").
#' The function calculates the median day difference between consecutive dates.
#'
#' @param .data A dataset containing a column named \code{ref_date} with date values.
#'
#' @return A single character scalar: \code{"year"}, \code{"quarter"},
#'   \code{"month"}, \code{"week"}, or \code{"day"}.
#'
#' @export
#'
#' @examples
#' # Monthly reporting dates
#' data <- data.frame(
#'  ref_date = seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "months")
#' )
#'
#' guess_date_frequency(data)
#' #> [1] "month"
#' @importFrom stats median
guess_date_frequency <- function(.data) {
  ref_date <- .data[["ref_date"]] |>
    unique() |>
    sort()

  median_days <- median(diff(as.Date(ref_date)), na.rm = TRUE)

  if (median_days >= 360) {
    return("year")
  }
  if (median_days >= 80) {
    return("quarter")
  }
  if (median_days >= 27) {
    return("month")
  }
  if (median_days >= 6) {
    return("week")
  }
  return("day")
}

#' Build grouping variable choices for wagebill UI
#'
#' Constructs a named list of grouping choices for use in a Shiny selectInput,
#' grouped by dictionary module. Always includes an "All" option mapped to
#' \code{ref_date}.
#'
#' @param data A data frame.
#'
#' @return A named list of choices available in the data.
#'
#' @importFrom dplyr filter summarise pull
#' @importFrom purrr set_names
#'
#' @examples
#' example_data <- data.frame(
#' "ref_date" = as.Date(c("2020-01-01", "2020-02-01")),
#' "personnel_id" = c(1, 2),
#' "gross_salary_lcu" = c(1000, 1500)
#' )
#'
#' choices <- build_wagebill_group_choices(example_data)
#'
#' @export
build_wagebill_group_choices <- function(data) {
  module_choices <- govhr::dictionary |>
    dplyr::filter(
      .data[["variable_id"]] %in% names(data),
      .data[["variable_class"]] == "character",
      !.data[["variable_id"]] %in% c("ref_date", "contract_id", "personnel_id")
    ) |>
    dplyr::summarise(
      choices = list(purrr::set_names(
        .data[["variable_id"]],
        .data[["variable_name"]]
      )),
      .by = "module"
    ) |>
    dplyr::pull(.data[["choices"]], name = .data[["module"]])

  c(list("All" = "ref_date"), module_choices)
}

#' Render Movement Value Box
#'
#' @param .data A data frame containing personnel data.
#' @param type_movement A character string indicating the type of movement: "hire", "fire", or "turnover".
#'
#' @return A Shiny UI output for the movement value box.
#'
#' @importFrom shiny renderUI
#' @importFrom bslib value_box
#' @importFrom bsicons bs_icon
render_movement_box <- function(.data, type_movement) {
  movement_count <- generate_movement_data(
    .data = .data,
    movement_type = type_movement,
    measurement_type = "count",
    group_cols = "ref_date"
  )

  movement_rate <- generate_movement_data(
    .data = .data,
    movement_type = type_movement,
    measurement_type = "rate",
    group_cols = "ref_date"
  )

  latest_count <- movement_count |>
    na.omit() |>
    dplyr::filter(.data[["ref_date"]] == max(.data[["ref_date"]])) |>
    dplyr::pull(.data[["indicator"]])

  latest_rate <- movement_rate |>
    na.omit() |>
    dplyr::filter(.data[["ref_date"]] == max(.data[["ref_date"]])) |>
    dplyr::pull(.data[["indicator"]])

  renderUI({
    value_box(
      title = paste0(
        toupper(substr(type_movement, 1, 1)),
        substr(type_movement, 2, nchar(type_movement)),
        " (", format(max(.data[["ref_date"]]), "%b %Y"), ")"
      ),
      theme = value_box_theme(bg = "#C34729", fg = "#ffffff"),
      class = "border",
      max_height = "150px",
      value = switch(
        type_movement,
        hire = tagList(
          h5(paste("Count:", latest_count)),
          h5(paste("Rate:", round(latest_rate, 3), "%"))
        ),
        fire = tagList(
          h5(paste("Count:", latest_count)),
          h5(paste("Rate:", round(latest_rate, 3), "%"))
        ),
        retirement = tagList(
          h5(paste("Count:", latest_count)),
          h5(paste("Rate:", round(latest_rate, 3), "%"))
        ),
        turnover = tagList(
          h5("Ratio of Hires"),
          h5(paste("to Exits:", round(latest_rate, 3)))
        )
      ),
      showcase = switch(
        type_movement,
        hire = bsicons::bs_icon("person-plus-fill"),
        fire = bsicons::bs_icon("person-dash-fill"),
        retirement = bsicons::bs_icon("person-badge-fill"),
        turnover = bsicons::bs_icon("arrow-repeat")
      ),
      bslib::popover(
        bsicons::bs_icon("info-circle-fill"),
        switch(
          type_movement,
          hire = "Number and share of personnel hired in the most recent reference period.",
          fire = "Number and share of personnel separated (voluntary and involuntary) in the most recent reference period.",
          retirement = "Number and share of personnel retired in the most recent reference period.",
          turnover = "Ratio of hires to separations (including retirements) in the most recent reference period."
        )
      )
    )
  })
}
