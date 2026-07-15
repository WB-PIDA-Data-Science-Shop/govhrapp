
#' Classify Personnel Events
#'
#' This function classifies the personnel module into three types of events: hires, fires, or retirements.
#'
#' @param .data A data frame containing personnel data.
#' @param id_col The name of the column representing personnel IDs.
#' @param event_type The type of event to classify (e.g., "hire", "fire", and "retirement").
#' @param start_date The start date for the classification period.
#' @param end_date The end date for the classification period.
#' @param status_col The name of the column representing employment status.
#' @param freq The frequency of the reference dates (default is "year").
#'
#' @importFrom data.table setDT fcase copy
#' @importFrom lubridate ymd
#' @importFrom govhr detect_personnel_event
#'
#' @return A data frame with an additional column indicating the type of event for each personnel record.
classify_personnel_event <- function(
  .data,
  id_col,
  event_type,
  start_date,
  end_date,
  status_col,
  freq = "year"
) {
  if(event_type %in% c("hire", "fire")) {
    personnel_event <- govhr::detect_personnel_event(
      data = .data,
      event_type = event_type,
      id_col = id_col,
      start_date = start_date,
      end_date = end_date,
      status_col = status_col,
      freq = freq
    )
  } else if(event_type == "retirement") {
    personnel_event <- govhr::detect_retirement(.data)
  }

  .data <- data.table::copy(setDT(.data))
  personnel_event <- data.table::setDT(personnel_event)

  .data[personnel_event, on = c(id_col, "ref_date"), type_event := i.type_event]

  .data[,
    type_event := fcase(
      type_event == "hire" , "hire" ,
      type_event == "fire" , "fire" ,
      type_event == "retire" , "retirement" ,
      default = "stayed"
    )
  ]

  # exclude minimum ref_date when movement_type is hire
  # and exclude maximum ref_date when movement_type is fire
  if (event_type == "hire") {
    .data <- .data[ref_date > lubridate::ymd(start_date)]
  } else if (event_type == "fire") {
    .data <- .data[ref_date < lubridate::ymd(end_date)]
  }

  .data[]
}

#' Project Retirement Dates
#' @details The function takes a data frame containing personnel data with birth dates and reference dates, calculates the projected retirement date for each staff member based on the specified threshold age, and counts the number of staff eligible for retirement at each reference date.
#' @param workforce_data A data frame containing personnel data with birth dates and reference dates.
#' @param threshold_age The age at which personnel are considered eligible for retirement (default is 60).
#' @param birth_col The name of the column representing birth dates (default is "birth_date").
#' @param group_cols A character vector of column names to group the data by when counting eligible retirees (default is NULL, meaning no grouping).
#' @param simplify_retirement_date A logical value indicating whether to simplify the retirement date to the end of the year (default is TRUE).
#' 
#' @return A data frame with projected retirement dates and counts of staff eligible for retirement at each reference date.
#' 
#' @importFrom data.table as.data.table
project_retirement <- function(
  workforce_data, 
  threshold_age = 60, 
  birth_col = "birth_date",
  group_cols = NULL,
  simplify_retirement_date = TRUE
) {
  workforce_data_dt <- as.data.table(workforce_data)

  # extract last record for each personnel_id
  # this raises an issue for time-variant columns such as education
  workforce_data_dt <- workforce_data_dt[, .SD[.N], by = "personnel_id"]

  # project retirement date for each staff member based on threshold_age
  workforce_data_dt[, retirement_date := as.Date(paste0(
    as.integer(format(get(birth_col), "%Y")) + threshold_age,
    format(get(birth_col), "-%m-%d")
  ))]

  # retain only projected retirements after last reference date in the data
  workforce_data_dt <- workforce_data_dt[retirement_date > max(workforce_data[["ref_date"]])]

  if(simplify_retirement_date) {
    workforce_data_dt[, retirement_date := as.Date(paste0(
      as.integer(format(retirement_date, "%Y")),
      "-12-31"
    ))]
  }

  # count number of staff eligible for retirement at each reference date
  if (is.null(group_cols) || identical(group_cols, "ref_date")) {
    projected_retirement_data <- workforce_data_dt[, .(indicator = .N), by = retirement_date][
      order(retirement_date)
    ]
  } else {
    projected_retirement_data <- workforce_data_dt[, .(indicator = .N), by = c("retirement_date", group_cols)][
      order(retirement_date)
    ]
  }

  projected_retirement_data[]
}