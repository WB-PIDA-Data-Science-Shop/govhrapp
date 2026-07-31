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
  start_ref_date <- lubridate::ymd(start_date)
  end_ref_date <- lubridate::ymd(end_date)

  if (event_type == "hire") {
    .data <- .data[ref_date > start_ref_date]
  } else if (event_type == "fire") {
    .data <- .data[ref_date < end_ref_date]
  }

  .data[]
}

#' Project Retirement Dates
#' @details The function takes a data frame containing personnel data with birth dates and reference dates. It only considers the last reference date in the data. It then calculates the projected retirement date for each staff member based on the specified threshold age, and counts the number of staff eligible for retirement at each future reference date.
#' @param .data A data frame, either the workforce or wage bill data.
#' @param threshold_age The age at which personnel are considered eligible for retirement (default is 60).
#' @param birth_col The name of the column representing birth dates (default is "birth_date").
#' @param group_cols A character vector of column names to group the data by when counting eligible retirees (default is NULL, meaning no grouping).
#' @param measure_col The name of the column representing the measure to be projected (default is NULL, meaning no measure column).
#' @param retirement_coefficient A numeric value indicating the coefficient to apply to the projected retirement cost (default is 0.6).
#' @param simplify_retirement_date A logical value indicating whether to simplify the retirement date to the end of the year (default is TRUE).
#' @param cutoff_date A numeric value indicating the cut-off for future retirement projections in years (default is 10).
#'
#' @return A data frame with projected retirement dates and counts of staff eligible for retirement at each reference date.
#'
#' @importFrom data.table as.data.table
project_retirement <- function(
  .data,
  threshold_age = 60,
  birth_col = "birth_date",
  group_cols = NULL,
  measure_col = NULL,
  retirement_coefficient = 0.6,
  simplify_retirement_date = TRUE,
  cutoff_date = 10
) {
  data_dt <- as.data.table(.data)

  # future extension: (a) incorporate threshold_tenure (b) enable user to choose which reference date to use as a baseline for projection.

  # extract last record for each personnel_id
  # this raises an issue for time-variant columns such as education
  data_dt <- data_dt[, .SD[.N], by = "personnel_id"]

  # project retirement date for each staff member based on threshold_age
  data_dt[,
    retirement_date := as.Date(paste0(
      as.integer(format(get(birth_col), "%Y")) + threshold_age,
      format(get(birth_col), "-%m-%d")
    ))
  ]

  # retain only projected retirements after last reference date in the data
  data_dt <- data_dt[
    retirement_date > max(.data[["ref_date"]])
  ]

  if (simplify_retirement_date) {
    data_dt[,
      retirement_date := as.Date(paste0(
        as.integer(format(retirement_date, "%Y")),
        "-12-31"
      ))
    ]
  }

  # count number of staff eligible for retirement at each retirement date
  if (is.null(group_cols) || group_cols == "ref_date") {
    projected_retirement_data <- data_dt[,
      .(indicator = .N),
      by = retirement_date
    ][
      order(retirement_date)
    ]
  } else {
    projected_retirement_data <- data_dt[,
      .(indicator = .N),
      by = c("retirement_date", group_cols)
    ][
      order(retirement_date)
    ]
  }

  if (!is.null(measure_col)) {
    projected_cost_dt <- data_dt[,
      .(projected_cost = sum(get(measure_col), na.rm = TRUE) * retirement_coefficient),
      by = retirement_date
    ]

    # join projected cost to projected retirement data
    projected_retirement_data <- merge(
      projected_retirement_data,
      projected_cost_dt,
      by = "retirement_date"
    )
  }

  # cut-off date
  projected_retirement_data <- projected_retirement_data[
    retirement_date <=
      (max(.data[["ref_date"]]) + lubridate::years(cutoff_date))
  ]

  projected_retirement_data[]
}

#' Function to compute quantiles of a measure column within groups and reference dates.
#'
#' @param .data A data frame containing the data to be processed.
#' @param group_cols A character vector of column names to group the data by.
#' @param measure_col The name of the column for which quantiles will be computed.
#' @param latest_measure A logical value indicating whether to return only the measures for the latest reference date's quantiles (default is FALSE).
#' @param n_quantiles The number of quantiles to compute (default is 10 for deciles).
#' 
#' @return A data frame containing the quantiles, median values, and mean values for the specified measure column within the specified groups and reference dates.
#'
#' @importFrom data.table as.data.table setorderv
#' @importFrom collapse fquantile
#'
#' @export
compute_quantile <- function(
  .data,
  group_cols = NULL,
  measure_col,
  latest_measure = FALSE,
  n_quantiles = 10
) {
  dt <- data.table::as.data.table(.data)

  # change group_cols based on the choice of latest measure
  by_cols <- if (latest_measure) {
    group_cols
  } else {
    c(group_cols, "ref_date")
  }

  if (latest_measure) {
    dt <- dt[ref_date == max(ref_date)]
  }

  dt[, decile := collapse::fquantile(get(measure_col), probs = seq(0, 1, by = 1/n_quantiles)), by = by_cols]

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

#' Function to compute the compression ratio
#'
#' @param .data A data frame.
#' @param group_cols A character vector of column names to group the data by.
#' @param percentiles A numeric vector of length 3 indicating the upper, middle, and lower percentiles to compute (default is c(0.9, 0.5, 0.1)).
#' @param measure_col The name of the column for which the compression ratio will be computed.
#' @param latest_measure A logical value indicating whether to return only the measures for the latest reference date.
#'
#' @importFrom data.table as.data.table setorderv
#' @importFrom collapse fquantile
#'
#' @return A data frame containing the 90th, 50th, and 10th percentiles for the specified measure column within the specified groups and reference dates.
compute_compression_ratio <- function(
  .data,
  group_cols = NULL,
  percentiles = c(0.9, 0.5, 0.1),
  measure_col,
  latest_measure = FALSE
) {
  # consider generalizing this function to compute any percentile, not just 90th, 50th, and 10th
  dt <- data.table::as.data.table(.data)

  by_cols <- c(group_cols, "ref_date")

  out <- dt[
    !is.na(get(measure_col)),
    .(
      percentile_upper = collapse::fquantile(
        get(measure_col),
        probs = percentiles[1],
        na.rm = TRUE
      ),
      percentile_50 = collapse::fquantile(
        get(measure_col),
        probs = percentiles[2],
        na.rm = TRUE
      ),
      percentile_lower = collapse::fquantile(
        get(measure_col),
        probs = percentiles[3],
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

#' Function to compute the total cost associated with personnel movement events.
#'
#' @param .data A data frame containing the data to be processed.
#' @param id_col The name of the column representing personnel IDs (default is "personnel_id").
#' @param event_type A character vector indicating which movement event(s) to include (e.g., "hire", "fire", "retirement"). Multiple types can be supplied to compute costs for each type.
#' @param start_date The start date for the classification period. Defaults to the minimum reference date found in `.data`.
#' @param end_date The end date for the classification period. Defaults to the maximum reference date found in `.data`.
#' @param status_col The name of the column representing employment status (default is "employment_status").
#' @param freq The frequency of the reference dates. Defaults to a guess based on `.data`.
#' @param measure_col The name of the column containing the cost/measure to sum.
#' @param group_cols A character vector of column names to group the data by.
#' @param latest_measure A logical value indicating whether to return only the measures for the latest reference date.
#'
#' @importFrom data.table as.data.table setorderv rbindlist
#'
#' @return A data frame containing the movement cost for each requested event type within the specified groups and reference dates.
compute_movement_cost <- function(
  .data,
  id_col = "personnel_id",
  event_type,
  start_date = NULL,
  end_date = NULL,
  status_col = "employment_status",
  freq = NULL,
  measure_col,
  group_cols = NULL,
  latest_measure = FALSE
) {
  dt <- data.table::as.data.table(.data)

  if (is.null(start_date)) {
    start_date <- as.character(min(dt[["ref_date"]]))
  }
  if (is.null(end_date)) {
    end_date <- as.character(max(dt[["ref_date"]]))
  }
  if (is.null(freq)) {
    freq <- guess_date_frequency(dt)
  }

  by_cols <- c(group_cols, "ref_date")

  out <- data.table::rbindlist(
    lapply(event_type, function(type) {
      # classify personnel events
      classified <- classify_personnel_event(
        .data = dt,
        id_col = id_col,
        event_type = type,
        start_date = start_date,
        end_date = end_date,
        status_col = status_col,
        freq = freq
      )

      # compute movement cost
      classified[
        type_event == type,
        .(
          movement_type = type,
          measurement = measure_col,
          movement_cost = sum(get(measure_col), na.rm = TRUE)
        ),
        keyby = by_cols
      ]
    })
  )

  data.table::setorderv(out, "ref_date")

  if (latest_measure) {
    latest_ref_date <- max(out[["ref_date"]])

    out <- out[ref_date == latest_ref_date]
  }

  out[]
}

#' Function to compute the cumulative distribution function of a variable.
#'
#' @param .data A data frame.
#' @param group_col A character vector of column names to group the data by.
#' @param measure_col The name of the column for which the percentile values will be computed.
#' @param binwidth The width of the bins for grouping the measure values (default is 1).
#' @param latest_measure A logical value indicating whether to return only the measures for the latest reference date.
#'
#' @importFrom data.table as.data.table setorderv
#' @importFrom collapse fquantile
#'
#' @return A data frame with the cumulative distribution function.
compute_cumulative <- function(
  .data,
  group_col = NULL,
  measure_col,
  binwidth = 1,
  latest_measure = FALSE
) {
  if (latest_measure) {
    .data <- .data[.data[["ref_date"]] == max(.data[["ref_date"]]), ]
  }

  dt <- data.table::as.data.table(.data)
  dt[, bin := floor(get(measure_col) / binwidth) * binwidth]
  dt <- dt[!is.na(bin)]

  binned <- dt[, .(count = .N), by = c(group_col, "bin")]

  # full grid of every bin in range, crossed with every group present
  all_bins <- seq(min(dt$bin), max(dt$bin), by = binwidth)

  full_grid <- if (is.null(group_col)) {
    data.table::data.table(bin = all_bins)
  } else {
    data.table::CJ(
      unique(dt[[group_col]]),
      all_bins,
      sorted = FALSE
    ) |>
      data.table::setnames(c(group_col, "bin"))
  }

  binned <- merge(full_grid, binned, by = c(group_col, "bin"), all.x = TRUE)
  binned[is.na(count), count := 0L]

  data.table::setorderv(binned, c(group_col, "bin"))

  binned <- binned[,
    c(
      .SD,
      list(
        pct = count / sum(count),
        cum_pct = cumsum(count) / sum(count)
      )
    ),
    by = group_col
  ]

  binned[]
}


#' Compute time trend 
#'
#' Summarizes data over time by grouping variable, producing a tidy data frame
#' with `ref_date`, optional group column, and `value`.
#'
#' When `measure_col` is `NULL`, counts rows per period (headcount). When a
#' column name is supplied, sums that column per period (wage bill).
#'
#' @param data A data frame containing at least a `ref_date` column.
#' @param group Character string naming the grouping column, or `"ref_date"` for
#'   no grouping.
#' @param measure_col Character string naming the numeric column to sum, or
#'   `NULL` to count rows.
#'
#' @return A summarized data frame with columns `ref_date`, optionally `group`, and `value`. Value denotes either a sum or headcount (if `measure_col` is `NULL`).
#'
#' @importFrom data.table as.data.table
#' @importFrom govhr compute_fastsummary
#' @export
compute_time_trend <- function(.data, group, measure_col = NULL) {
  .data_dt <- data.table::as.data.table(.data)

  groups <- if (group == "ref_date") "ref_date" else c("ref_date", group)

  if (is.null(measure_col)) {
    # headcount by group
    .data_dt <- .data_dt[, .(value = .N), by = groups]

    # order by groups
    data.table::setorderv(.data_dt, groups)

    .data_dt
  } else {
    .data_dt |>
      govhr::compute_fastsummary(
        cols = measure_col,
        fns = "sum",
        groups = groups
      )
  }
}

#' Apply Baseline Index to Trend Summary
#'
#' Rescales the `value` column so that the first observation equals 100,
#' producing a baseline index. When a grouping variable is present, the
#' rescaling is applied within each group.
#'
#' @param data A data frame with columns `ref_date` and `value`, as returned by
#'   [compute_time_trend()].
#' @param group Character string naming the grouping column, or `"ref_date"` for
#'   no grouping.
#'
#' @return The input data frame with `value` rescaled to a baseline index.
#'
#' @importFrom dplyr arrange mutate across all_of ungroup first
#' @export
apply_baseline_index <- function(data, group) {
  if (group == "ref_date") {
    data |>
      dplyr::arrange(.data[["ref_date"]]) |>
      dplyr::mutate(
        value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
      )
  } else {
    data |>
      dplyr::arrange(.data[["ref_date"]]) |>
      dplyr::mutate(
        value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100,
        .by = dplyr::all_of(group)
      )
  }
}

#' Compute Cross-Section Summary
#'
#' Filters to the latest reference date within each group, then aggregates to
#' produce a per-group `value`. Used as the data source for total-by-group bar
#' charts.
#'
#' When `measure_col` is `NULL`, counts rows (headcount). When a column name is
#' supplied, sums that column (wage bill).
#'
#' @param data A data frame containing a `ref_date` column and the grouping
#'   column.
#' @param group Character string naming the grouping column.
#' @param measure_col Character string naming the numeric column to sum, or
#'   `NULL` to count rows.
#'
#' @return A data frame with the grouping column and a `value` column.
#'
#' @importFrom dplyr group_by across all_of filter ungroup summarise n
#' @importFrom govhr compute_fastsummary
#' @export
compute_cross_section_summary <- function(data, group, measure_col = NULL) {
  # only consider latest reference date
  data_latest <- data |>
    dplyr::filter(
      .data[["ref_date"]] == max(.data[["ref_date"]]),
      .by = dplyr::all_of(group)
    )

  if (is.null(measure_col)) {
    data_latest |>
      dplyr::summarise(value = dplyr::n(), .by = dplyr::all_of(group))
  } else {
    data_latest |>
      govhr::compute_fastsummary(
        cols = measure_col,
        fns = "sum",
        groups = group
      )
  }
}

#' Compute Growth Rate Summary
#'
#' Filters to the first and last reference date within each group and computes
#' the percentage change from first `ref_date` to last `ref_date`.
#'
#' When `measure_col` is `NULL`, counts rows per date-group cell (headcount).
#' When a column name is supplied, sums that column (wage bill).
#'
#' @param data A data frame with `ref_date` and the grouping column.
#' @param group Character string naming the grouping column.
#' @param measure_col Character string naming the numeric column to sum, or
#'   `NULL` to count rows.
#'
#' @return A data frame with the grouping column and a `growth_rate` column
#'   (percentage points, e.g. 12.5 for +12.5%).
#'
#' @importFrom dplyr group_by across all_of filter ungroup summarise n first last
#' @importFrom govhr compute_fastsummary
#' @export
compute_growth_summary <- function(data, group, measure_col = NULL) {
  endpoints <- data |>
    dplyr::filter(
      .data[["ref_date"]] %in%
        c(max(.data[["ref_date"]]), min(.data[["ref_date"]])),
      .by = dplyr::all_of(group)
    ) |>
    dplyr::arrange(.data[["ref_date"]])

  summarized <- if (is.null(measure_col)) {
    endpoints |>
      dplyr::summarise(
        value = dplyr::n(),
        .by = dplyr::all_of(c("ref_date", group))
      )
  } else {
    endpoints |>
      govhr::compute_fastsummary(
        cols = measure_col,
        fns = "sum",
        groups = c("ref_date", group)
      )
  }

  summarized |>
    dplyr::filter(!is.na(.data[[group]])) |>
    dplyr::summarise(
      growth_rate = round(
        dplyr::last(.data[["value"]]) / dplyr::first(.data[["value"]]) - 1,
        3
      ) *
        100,
      .by = dplyr::all_of(group)
    ) |>
    dplyr::filter(!is.na(.data[["growth_rate"]]))
}

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