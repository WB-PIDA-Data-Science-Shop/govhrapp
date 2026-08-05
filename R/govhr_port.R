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

#' Function to compute the percentile values
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
#' @return A data frame containing the 90th, 50th, and 10th percentiles for the specified measure column within the specified groups and reference dates.
compute_percentile <- function(
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
