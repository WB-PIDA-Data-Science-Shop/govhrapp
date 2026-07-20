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
  if (event_type %in% c("hire", "fire")) {
    personnel_event <- govhr::detect_personnel_event(
      data = .data,
      event_type = event_type,
      id_col = id_col,
      start_date = start_date,
      end_date = end_date,
      status_col = status_col,
      freq = freq
    )
  } else if (event_type == "retirement") {
    personnel_event <- govhr::detect_retirement(.data)
  }

  .data <- data.table::copy(setDT(.data))
  personnel_event <- data.table::setDT(personnel_event)

  .data[personnel_event, on = c(id_col, "ref_date"), type_event := i.type_event]

  .data[,
    type_event := fcase(
      type_event == "hire"   , "hire"       ,
      type_event == "fire"   , "fire"       ,
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
#' @param cutoff_date A numeric value indicating the cut-off for future retirement projections in years (default is 10).
#'
#' @return A data frame with projected retirement dates and counts of staff eligible for retirement at each reference date.
#'
#' @importFrom data.table as.data.table
project_retirement <- function(
  workforce_data,
  threshold_age = 60,
  birth_col = "birth_date",
  group_cols = NULL,
  simplify_retirement_date = TRUE,
  cutoff_date = 10
) {
  workforce_data_dt <- as.data.table(workforce_data)

  # future extension: incorporate threshold_tenure

  # extract last record for each personnel_id
  # this raises an issue for time-variant columns such as education
  workforce_data_dt <- workforce_data_dt[, .SD[.N], by = "personnel_id"]

  # project retirement date for each staff member based on threshold_age
  workforce_data_dt[,
    retirement_date := as.Date(paste0(
      as.integer(format(get(birth_col), "%Y")) + threshold_age,
      format(get(birth_col), "-%m-%d")
    ))
  ]

  # retain only projected retirements after last reference date in the data
  workforce_data_dt <- workforce_data_dt[
    retirement_date > max(workforce_data[["ref_date"]])
  ]

  if (simplify_retirement_date) {
    workforce_data_dt[,
      retirement_date := as.Date(paste0(
        as.integer(format(retirement_date, "%Y")),
        "-12-31"
      ))
    ]
  }

  # count number of staff eligible for retirement at each retirement date
  if (is.null(group_cols) || identical(group_cols, "ref_date")) {
    projected_retirement_data <- workforce_data_dt[,
      .(indicator = .N),
      by = retirement_date
    ][
      order(retirement_date)
    ]
  } else {
    projected_retirement_data <- workforce_data_dt[,
      .(indicator = .N),
      by = c("retirement_date", group_cols)
    ][
      order(retirement_date)
    ]
  }

  # cut-off date
  projected_retirement_data <- projected_retirement_data[
    retirement_date <=
      (max(workforce_data[["ref_date"]]) + lubridate::years(cutoff_date))
  ]

  projected_retirement_data[]
}

#' Function to compute deciles of a measure column within groups and reference dates.
#'
#' @param .data A data frame containing the data to be processed.
#' @param group_cols A character vector of column names to group the data by.
#' @param measure_col The name of the column for which deciles will be computed.
#' @param latest_measure A logical value indicating whether to return only the measures for the latest reference date's deciles (default is FALSE).
#'
#' @return A data frame containing the deciles, median values, and mean values for the specified measure column within the specified groups and reference dates.
#'
#' @importFrom data.table as.data.table setorderv
#' @importFrom dplyr ntile
#'
#' @export
compute_decile <- function(
  .data,
  group_cols = NULL,
  measure_col,
  latest_measure = FALSE
) {
  dt <- data.table::as.data.table(.data)

  by_cols <- if (latest_measure) {
    group_cols
  } else {
    c(group_cols, "ref_date")
  }

  if (latest_measure) {
    dt <- dt[ref_date == max(ref_date)]
  }

  dt[, decile := dplyr::ntile(get(measure_col), 10), by = by_cols]

  out <- dt[
    !is.na(decile),
    .(
      median_value = stats::median(get(measure_col), na.rm = TRUE),
      mean_value = mean(get(measure_col), na.rm = TRUE)
    ),
    keyby = c(by_cols, "decile")
  ]

  data.table::setorderv(out, c(by_cols, "decile"))

  out[]
}

compute_compression_ratio <- function(
  .data,
  group_cols = NULL,
  measure_col,
  latest_measure = FALSE
) {
  dt <- data.table::as.data.table(.data)

  by_cols <- c(group_cols, "ref_date")

  out <- dt[
    !is.na(get(measure_col)),
    .(
      percentile_90 = collapse::fquantile(
        get(measure_col),
        probs = 0.9,
        na.rm = TRUE
      ),
      percentile_50 = collapse::fquantile(
        get(measure_col),
        probs = 0.5,
        na.rm = TRUE
      ),
      percentile_10 = collapse::fquantile(
        get(measure_col),
        probs = 0.1,
        na.rm = TRUE
      )
    ),
    keyby = by_cols
  ]

  if (latest_measure && group_cols != "ref_date") {
    out <- out[ref_date == max(ref_date)]
  }

  data.table::setorderv(out, by_cols)

  out[]
}

compute_adjustment_cost <- function(
  .data,
  id_col = "personnel_id",
  event_type,
  start_date,
  end_date,
  status_col = "employment_status",
  freq = "year",
  measure_col,
  group_cols = NULL
) {
  dt <- data.table::as.data.table(.data)

  # classify personnel events
  dt <- classify_personnel_event(
    .data = dt,
    id_col = id_col,
    event_type = event_type,
    start_date = start_date,
    end_date = end_date,
    status_col = status_col,
    freq = freq
  )

  # compute adjustment cost
  by_cols <- c(group_cols, "ref_date")

  out <- dt[
    type_event == event_type,
    .(
      movement_type = event_type,
      measurement = measure_col,
      adjustment_cost = sum(get(measure_col), na.rm = TRUE)
    ),
    keyby = by_cols
  ]

  data.table::setorderv(out, "ref_date")

  out[]
}
