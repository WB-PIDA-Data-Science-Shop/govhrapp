#' Compute Deciles of a Measure
#'
#' Assigns rows to deciles of `measure_col` within each group and reference
#' date, then reports the median and mean of the measure in each decile.
#'
#' @param .data Data frame containing a `ref_date` column and the measure.
#' @param group_cols Character vector of columns to group by, or `NULL` for no
#'   grouping.
#' @param measure_col Character. Numeric column to rank into deciles.
#' @param latest_measure Logical. Restrict to the latest reference date and drop
#'   `ref_date` from the grouping. Default `FALSE`.
#'
#' @return A data frame with the grouping columns, `decile`, `median_value` and
#'   `mean_value`.
#'
#' @importFrom data.table as.data.table setorderv
#' @importFrom dplyr ntile
#' @importFrom stats median
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

#' Bin a Measure into a Share Distribution
#'
#' Bins `measure_col` at a fixed width and reports each bin's share and
#' cumulative share of observations, filling empty bins with zero so the
#' distribution is gap-free.
#'
#' @param .data Data frame containing a `ref_date` column and the measure.
#' @param group_col Character. Column to group by, or `NULL` for no grouping.
#' @param measure_col Character. Numeric column to bin.
#' @param binwidth Numeric. Width of each bin. Default `1`.
#' @param latest_measure Logical. Restrict to the latest reference date. Default
#'   `FALSE`.
#'
#' @return A data frame with the grouping column, `bin`, `count`, `pct` and
#'   `cum_pct`.
#'
#' @importFrom data.table CJ as.data.table data.table setnames setorderv
#' @keywords internal
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


compute_compression_ratio <- function(
  .data,
  group_cols = NULL,
  percentiles = c(0.9, 0.5, 0.1),
  measure_col
) {
  # consider generalizing this function to compute any percentile, not just 90th, 50th, and 10th
  dt <- data.table::as.data.table(.data)

  by_cols <- group_cols

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

  data.table::setorderv(out, by_cols)

  out[]
}

#' Count Unique Entities by Group
#' 
#' @param .data Data frame containing the data.
#' @param id_col Character. Column name of the unique identifier for the entity.
#' @param group_cols Character vector of column names to group by, or `NULL` for no grouping.
#' 
#' @return A data frame with the grouping columns and a `count` column representing the number of unique entities in each group.
#' 
#' @importFrom data.table as.data.table uniqueN setorderv
#' @export
count_entity <- function(.data, id_col, group_cols = NULL){
  dt <- data.table::as.data.table(.data)

  out <- dt[
    !is.na(get(id_col)),
    .(
      count = data.table::uniqueN(get(id_col))
    ),
    keyby = group_cols
  ]

  data.table::setorderv(out, c(group_cols, "count"))

  out[]
}