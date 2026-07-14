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

generate_movement_data <- function(.data, movement_type, measurement_type, group_cols) {
  min_date <- min(.data[["ref_date"]]) |>
    as.character()
  max_date <- max(.data[["ref_date"]]) |>
    as.character()

  agg_fun <- switch(
    measurement_type,
    count = sum,
    rate = mean,
    stop("Invalid measurement_type. Must be 'count' or 'rate'.")
  )

  # extract frequency of reference dates for movements
  freq_ref_date <- .data |>
    guess_date_frequency()

  if (movement_type %in% c("hire", "fire")) {
    movement_data <- .data |>
      govhr::detect_personnel_event(
        event_type = movement_type,
        id_col = "personnel_id",
        start_date = min_date,
        end_date = max_date,
        status_col = "employment_status",
        freq = freq_ref_date
      ) |>
      dplyr::right_join(
        .data,
        by = c("personnel_id", "ref_date")
      ) |>
      summarise(
        indicator = agg_fun(!is.na(.data[["type_event"]])),
        .by = dplyr::all_of(
          unique(c("ref_date", group_cols))
        )
      )
  } else {
    hire_data <- .data |>
      govhr::detect_personnel_event(
        event_type = "hire",
        id_col = "personnel_id",
        start_date = min_date,
        end_date = max_date,
        status_col = "employment_status",
        freq = freq_ref_date
      ) |>
      dplyr::left_join(
        .data,
        by = c("personnel_id", "ref_date")
      ) |>
      summarise(
        hires = n(),
        .by = dplyr::all_of(
          unique(c("ref_date", group_cols))
        )
      )

    fire_data <- .data |>
      govhr::detect_personnel_event(
        event_type = "fire",
        id_col = "personnel_id",
        start_date = min_date,
        end_date = max_date,
        status_col = "employment_status",
        freq = freq_ref_date
      ) |>
      dplyr::left_join(
        .data,
        by = c("personnel_id", "ref_date")
      ) |>
      summarise(
        fires = n(),
        .by = dplyr::all_of(
          unique(c("ref_date", group_cols))
        )
      )

    movement_data <- hire_data |>
      left_join(
        fire_data,
        by = unique(c("ref_date", group_cols))
      ) |>
      mutate(
        indicator = .data[["hires"]] / .data[["fires"]]
      )
  }

  movement_data
}

#' Classify Personnel Events
#' 
#' This function classifies the personnel module into three types of events: hires, fires, or stays.
#' 
#' @param .data A data frame containing personnel data.
#' @param id_col The name of the column representing personnel IDs.
#' @param event_type The type of event to classify (e.g., "hire", "fire").
#' @param start_date The start date for the classification period.
#' @param end_date The end date for the classification period.
#' @param status_col The name of the column representing employment status.
#' @param freq The frequency of the reference dates (default is "year").
#' 
#' @importFrom data.table setDT fcase copy
#' @importFrom govhr detect_personnel_event
#' 
#' @return A data frame with an additional column indicating the type of event for each personnel record.
classify_personnel_event <- function(.data, id_col, event_type, start_date, end_date, status_col, freq = "year") {
  personnel_event <- .data |>
    govhr::detect_personnel_event(
      event_type = event_type,
      id_col = id_col,
      start_date = start_date,
      end_date = end_date,
      status_col = status_col
    ) 

  .data <- data.table::copy(setDT(.data))
  personnel_event <- data.table::setDT(personnel_event)
  
  .data[personnel_event, on = c(id_col, "ref_date"), type_event := i.type_event]

  .data[, type_event := fcase(
    type_event == "hire", "hired",
    type_event == "fire", "fired",
    default = "stayed"
  )]

  .data[]
}