#' Compute data coverage
#'
#' Computes, for each group, the share of non-missing values in every other
#' column, as a percentage. Optionally averages the shares across columns,
#' giving one coverage value per group.
#'
#' @param data Data frame or remote database table (`tbl_dbi`).
#' @param group_cols Character vector of columns to group by, or `NULL`
#'   (default) for no grouping.
#' @param include_ref_date Logical. Also group by `ref_date`. Default `FALSE`.
#' @param aggregate Logical. Average the coverage across columns, giving one
#'   value per group. Default `FALSE`.
#' @param ... Arguments passed to methods.
#'
#' @returns A table with the grouping columns, `variable` (the name of a
#'   column, unless `aggregate` is `TRUE`) and `coverage` (the share of
#'   non-missing values, from 0 to 100). A data.table, also for `tbl_dbi`
#'   input.
#'
#' @details
#' Every column that is not a grouping column is covered, identifiers
#' included. Missing groups are kept as their own group. With
#' `aggregate = TRUE`, every column weighs equally in the average.
#'
#' For `tbl_dbi` input, the coverage of every column is computed in the
#' database, giving one row per group, and only that summary is brought into
#' memory to be reshaped.
#'
#' This is a candidate to replace [govhr::compute_coverage()]. Unlike it, it
#' drops the deprecated `group` argument, returns a data.table rather than a
#' tibble for data frame input, and works on database tables.
#'
#' @seealso [govhr::compute_global_coverage()], which gives a single coverage
#'   value for the whole table. [govhr::plot_coverage_trend()],
#'   [govhr::plot_coverage_bar()] and [govhr::plot_coverage_heatmap()], which
#'   draw coverage.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(
#'   ref_date = as.Date(c("2020-01-01", "2020-01-01", "2021-01-01")),
#'   gender = c("F", NA, "M"),
#'   birth_date = as.Date(c("1980-01-01", NA, NA))
#' )
#' compute_coverage(hr, include_ref_date = TRUE)
#' }
#'
#' @export
compute_coverage <- function(data, ...) {
  UseMethod("compute_coverage")
}

#' @rdname compute_coverage
#' @importFrom data.table .SD as.data.table melt setorderv
#' @importFrom rlang check_dots_empty
#' @export
compute_coverage.data.frame <- function(
  data,
  group_cols = NULL,
  include_ref_date = FALSE,
  aggregate = FALSE,
  ...
) {
  rlang::check_dots_empty()

  if (include_ref_date) {
    group_cols <- unique(c("ref_date", group_cols))
  }

  dt <- data.table::as.data.table(data)
  value_cols <- setdiff(names(dt), group_cols)

  coverage_wide <- dt[
    , lapply(.SD, \(col) 100 * mean(!is.na(col))),
    by = group_cols,
    .SDcols = value_cols
  ]

  pivot_coverage(coverage_wide, group_cols, value_cols, aggregate)
}

#' @rdname compute_coverage
#' @importFrom dplyr across all_of collect if_else summarise
#' @importFrom rlang check_dots_empty
#' @export
compute_coverage.tbl_dbi <- function(
  data,
  group_cols = NULL,
  include_ref_date = FALSE,
  aggregate = FALSE,
  ...
) {
  rlang::check_dots_empty()

  if (include_ref_date) {
    group_cols <- unique(c("ref_date", group_cols))
  }
  
  value_cols <- setdiff(colnames(data), group_cols)

  # the wide summary has one row per group, so reshaping it in memory is cheap,
  # whereas reshaping in SQL takes dbplyr seconds to render for a few dozen
  # columns
  coverage_wide <- data |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(value_cols),
        \(col) 100 * mean(dplyr::if_else(is.na(col), 0, 1), na.rm = TRUE)
      ),
      .by = dplyr::all_of(group_cols)
    ) |>
    dplyr::collect()

  pivot_coverage(coverage_wide, group_cols, value_cols, aggregate)
}

# reshapes one row per group with a coverage column per variable into one row
# per group and variable, shared by both compute_coverage() methods
#' @importFrom data.table as.data.table melt setorderv
#' @keywords internal
#' @noRd
pivot_coverage <- function(coverage_wide, group_cols, value_cols, aggregate) {
  coverage <- data.table::melt(
    data.table::as.data.table(coverage_wide),
    id.vars = group_cols,
    measure.vars = value_cols,
    variable.name = "variable",
    value.name = "coverage",
    variable.factor = FALSE
  )

  if (aggregate) {
    coverage <- coverage[, .(coverage = mean(coverage)), by = group_cols]
  }

  # stable, so each group keeps its variables in column order
  if (!is.null(group_cols)) {
    data.table::setorderv(coverage, group_cols)
  }

  coverage[]
}

#' Plot coverage over time
#'
#' Draws coverage for each reference date, one line per group, from coverage
#' already computed.
#'
#' @param data Data frame with `ref_date`, `coverage` and the column named in
#'   `group_col`, such as the output of
#'   `compute_coverage(include_ref_date = TRUE, aggregate = TRUE)`.
#' @param group_col Character. Column to draw one line per group, or
#'   `"ref_date"` (default) for a single line.
#' @param toggle_growth Logical. Show coverage as a baseline index, with each
#'   group's first date at 100. Default `FALSE`.
#'
#' @returns A ggplot2 object.
#'
#' @details
#' This is a candidate to replace [govhr::plot_coverage_trend()]. Unlike it, it
#' takes computed coverage instead of computing it from raw data, so a cached
#' summary is not mistaken for raw data, and it indexes the values when
#' `toggle_growth` is `TRUE` rather than only relabelling the axis.
#'
#' @seealso [compute_coverage()], which computes the coverage.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(
#'   ref_date = as.Date(c("2020-01-01", "2020-01-01", "2021-01-01")),
#'   gender = c("F", NA, "M")
#' )
#' hr |>
#'   compute_coverage(include_ref_date = TRUE, aggregate = TRUE) |>
#'   plot_coverage_trend()
#' }
#'
#' @importFrom dplyr rename
#' @importFrom govhr apply_baseline_index plot_trend
#' @export
plot_coverage_trend <- function(
  data,
  group_col = "ref_date",
  toggle_growth = FALSE
) {
  # apply_baseline_index() and plot_trend() read the measure from `value`
  coverage <- dplyr::rename(data, value = "coverage")

  if (toggle_growth) {
    coverage <- govhr::apply_baseline_index(coverage, group_col = group_col)
  }

  govhr::plot_trend(
    coverage,
    group_col = group_col,
    toggle_growth = toggle_growth,
    y_label = "Coverage"
  )
}
