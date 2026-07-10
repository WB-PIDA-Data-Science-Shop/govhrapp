#' Compute coverage of non-missing values in a dataset.
#'
#' @param .data A data frame.
#' @param group A character string specifying the column name to group by. If NULL, coverage is computed for the entire data set.
#' @param include_ref_date A logical value indicating whether to include the `ref_date` column in the grouping.
#' @param aggregate A logical value indicating whether to aggregate coverage values by the `group`.
#'
#' @importFrom data.table as.data.table
#' @importFrom tibble as_tibble
#'
#' @return A data frame with coverage values for each column, optionally grouped by the specified `group`.
compute_coverage <- function(.data, group = NULL, include_ref_date = FALSE, aggregate = FALSE) {
  dt <- data.table::as.data.table(.data)
  data_cols <- colnames(dt)

  if (include_ref_date) {
    group <- unique(c("ref_date", group))
  }

  summary_cols <- setdiff(data_cols, group)

  # wide: one coverage value per summary column, one row per group
  if(is.null(group)){
    coverage_wide <- dt[
      ,
      lapply(.SD, \(col) (sum(!is.na(col)) / length(col)) * 100),
      .SDcols = summary_cols
    ]
  }else{
    coverage_wide <- dt[
      ,
      lapply(.SD, \(col) (sum(!is.na(col)) / length(col)) * 100),
      by = c(group),
      .SDcols = summary_cols
    ]
  }

  # long: pivot summary_cols into variable/coverage pairs
  coverage_data <- data.table::melt(
    coverage_wide,
    id.vars = group,
    measure.vars = summary_cols,
    variable.name = "variable",
    value.name = "coverage",
    variable.factor = FALSE
  )

  if (aggregate) {
    coverage_data <- coverage_data[
      ,
      .(coverage = mean(coverage, na.rm = TRUE)),
      by = c(group)
    ]
  }

  tibble::as_tibble(coverage_data)
}