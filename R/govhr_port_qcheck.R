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

#' Compute global coverage
#'
#' Computes the share of non-missing values across every cell of a table, as a
#' percentage.
#'
#' @param data Data frame or remote database table (`tbl_dbi`).
#' @param digits Whole number. Decimal places to round to. Default `2`.
#'
#' @returns A number from 0 to 100.
#'
#' @details
#' Every column has the same number of records, so the share across all cells
#' is the average of each column's coverage from [compute_coverage()], which
#' is computed in the database for `tbl_dbi` input.
#'
#' This is a candidate to replace [govhr::compute_global_coverage()]. Unlike
#' it, it works on database tables.
#'
#' @seealso [compute_coverage()], which gives the coverage of each column.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(gender = c("F", NA, "M"), grade = c(NA, NA, "G1"))
#' compute_global_coverage(hr)
#' }
#'
#' @export
compute_global_coverage <- function(data, digits = 2) {
  coverage <- compute_coverage(data)

  round(mean(coverage[["coverage"]]), digits)
}

#' Plot coverage by variable
#'
#' Draws one bar per variable with its coverage, coloured by coverage tier,
#' from coverage already computed.
#'
#' @param data Data frame with `variable` and `coverage`, such as the output
#'   of [compute_coverage()] without grouping.
#'
#' @returns A ggplot2 object.
#'
#' @details
#' Coverage below 50% is low, from 50% to 79% medium, and from 80% high.
#'
#' This is a candidate to replace [govhr::plot_coverage_bar()]. Unlike it, it
#' takes computed coverage instead of computing it from raw data.
#'
#' @seealso [compute_coverage()], which computes the coverage.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(gender = c("F", NA, "M"), grade = c(NA, NA, "G1"))
#' plot_coverage_bar(compute_coverage(hr))
#' }
#'
#' @importFrom dplyr case_when filter mutate
#' @importFrom ggplot2 aes geom_col ggplot labs scale_fill_manual
#'   scale_x_continuous
#' @importFrom scales label_percent
#' @importFrom stats reorder setNames
#' @importFrom stringr str_wrap
#' @export
plot_coverage_bar <- function(data) {
  coverage_tiers <- c("Low (<50%)", "Medium (50-79%)", "High (>=80%)")

  data |>
    dplyr::filter(!is.na(.data[["coverage"]])) |>
    dplyr::mutate(
      coverage_tier = dplyr::case_when(
        .data[["coverage"]] < 50 ~ coverage_tiers[1],
        .data[["coverage"]] < 80 ~ coverage_tiers[2],
        TRUE ~ coverage_tiers[3]
      ),
      coverage_tier = factor(.data[["coverage_tier"]], levels = coverage_tiers)
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["coverage"]],
        y = stats::reorder(
          stringr::str_wrap(.data[["variable"]], width = 30),
          .data[["coverage"]]
        ),
        fill = .data[["coverage_tier"]]
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(scale = 1),
      limits = c(0, 100)
    ) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(
        c("#d32f2f", "#f9a825", "#388e3c"),
        coverage_tiers
      ),
      drop = FALSE
    ) +
    ggplot2::labs(x = "Coverage", y = "", fill = "Coverage")
}

#' Plot coverage by variable and group
#'
#' Draws a heatmap of coverage, with groups across and variables down, from
#' coverage already computed.
#'
#' @param data Data frame with `variable`, `coverage` and the column named in
#'   `group_col`, such as the output of `compute_coverage(group_cols =
#'   group_col)`.
#' @param group_col Character. Column whose values run across the heatmap.
#'   Default `"ref_date"`.
#'
#' @returns A plotly object.
#'
#' @details
#' This is a candidate to replace [govhr::plot_coverage_heatmap()]. Unlike it,
#' it takes computed coverage instead of computing it from raw data.
#'
#' @seealso [compute_coverage()], which computes the coverage.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(
#'   ref_date = as.Date(c("2020-01-01", "2020-01-01", "2021-01-01")),
#'   gender = c("F", NA, "M")
#' )
#' plot_coverage_heatmap(compute_coverage(hr, group_cols = "ref_date"))
#' }
#'
#' @importFrom dplyr mutate
#' @export
plot_coverage_heatmap <- function(data, group_col = "ref_date") {
  data |>
    dplyr::mutate(coverage = .data[["coverage"]] / 100) |>
    plot_quality_heatmap(
      group_col = group_col,
      value_col = "coverage",
      value_label = "Coverage"
    )
}

#' Compute record consistency
#'
#' Computes, for each group, the share of identifiers that appear in exactly
#' one record per reference date, as a percentage.
#'
#' @param data Data frame or remote database table (`tbl_dbi`) with `ref_date`
#'   and the columns named in `id_col` and `group_cols`.
#' @param id_col Character. Column identifying the entity, such as
#'   `"personnel_id"`.
#' @param group_cols Character vector of columns to group by, such as
#'   `"ref_date"`, or `NULL` (default) for the whole table.
#' @param digits Whole number. Decimal places to round to. Default `2`.
#' @param ... Arguments passed to methods.
#'
#' @returns A table with the grouping columns and `record_consistency`, from 0
#'   to 100. A data.table for data frame input; a lazy table for `tbl_dbi`
#'   input (use [dplyr::collect()] to bring it into memory).
#'
#' @details
#' Records are counted per identifier, `ref_date` and group. A combination
#' with exactly one record is consistent, and `record_consistency` is the
#' share of consistent combinations in each group. Missing identifiers and
#' groups are kept as their own value.
#'
#' This is a candidate to replace [govhr::compute_record_consistency()].
#' Unlike it, it returns a data.table rather than a tibble for data frame
#' input, and works on database tables.
#'
#' @seealso [compute_value_consistency()], which checks that each identifier
#'   keeps the same value. [compute_global_consistency()], which combines
#'   both.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(
#'   personnel_id = c("a", "a", "b"),
#'   ref_date = as.Date("2020-01-01")
#' )
#' compute_record_consistency(hr, id_col = "personnel_id")
#' }
#'
#' @export
compute_record_consistency <- function(data, ...) {
  UseMethod("compute_record_consistency")
}

#' @rdname compute_record_consistency
#' @importFrom data.table .N as.data.table setorderv
#' @importFrom rlang check_dots_empty
#' @export
compute_record_consistency.data.frame <- function(
  data,
  id_col,
  group_cols = NULL,
  digits = 2,
  ...
) {
  rlang::check_dots_empty()

  dt <- data.table::as.data.table(data)
  count_cols <- unique(c(id_col, "ref_date", group_cols))

  records <- dt[, .(n_records = .N), by = count_cols]

  consistency <- records[
    , .(record_consistency = round(100 * mean(n_records == 1), digits)),
    by = group_cols
  ]

  if (!is.null(group_cols)) {
    data.table::setorderv(consistency, group_cols)
  }

  consistency[]
}

#' @rdname compute_record_consistency
#' @importFrom dplyr across all_of count if_else summarise
#' @importFrom rlang check_dots_empty
#' @export
compute_record_consistency.tbl_dbi <- function(
  data,
  id_col,
  group_cols = NULL,
  digits = 2,
  ...
) {
  rlang::check_dots_empty()

  count_cols <- unique(c(id_col, "ref_date", group_cols))

  data |>
    dplyr::count(dplyr::across(dplyr::all_of(count_cols)), name = "n_records") |>
    dplyr::summarise(
      # whole digits, which SQL's ROUND() requires
      record_consistency = round(
        100 * mean(dplyr::if_else(n_records == 1, 1, 0), na.rm = TRUE),
        !!as.integer(digits)
      ),
      .by = dplyr::all_of(group_cols)
    )
}

#' Compute value consistency
#'
#' Computes, for each group, the share of identifiers that keep a single value
#' of `value_col`, as a percentage.
#'
#' @inheritParams compute_record_consistency
#' @param value_col Character. Column whose values should stay the same for
#'   each identifier, such as `"birth_date"`.
#'
#' @returns A table with the grouping columns and `value_consistency`, from 0
#'   to 100. A data.table for data frame input; a lazy table for `tbl_dbi`
#'   input (use [dplyr::collect()] to bring it into memory).
#'
#' @details
#' Distinct values of `value_col` are counted per identifier and group, across
#' all dates unless `ref_date` is one of `group_cols`. An identifier with
#' exactly one distinct value is consistent. A missing value counts as a value
#' of its own, so an identifier recorded with and without a value is not
#' consistent.
#'
#' This is a candidate to replace [govhr::compute_value_consistency()].
#' Unlike it, it returns a data.table rather than a tibble for data frame
#' input, and works on database tables.
#'
#' @seealso [compute_record_consistency()], which checks that each identifier
#'   has one record per date. [compute_global_consistency()], which combines
#'   both.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(
#'   personnel_id = c("a", "a", "b"),
#'   ref_date = as.Date(c("2020-01-01", "2021-01-01", "2020-01-01")),
#'   birth_date = as.Date(c("1980-01-01", "1981-01-01", "1990-01-01"))
#' )
#' compute_value_consistency(hr, id_col = "personnel_id", value_col = "birth_date")
#' }
#'
#' @export
compute_value_consistency <- function(data, ...) {
  UseMethod("compute_value_consistency")
}

#' @rdname compute_value_consistency
#' @importFrom data.table .N as.data.table setorderv
#' @importFrom rlang check_dots_empty
#' @export
compute_value_consistency.data.frame <- function(
  data,
  id_col,
  value_col,
  group_cols = NULL,
  digits = 2,
  ...
) {
  rlang::check_dots_empty()

  dt <- data.table::as.data.table(data)
  by_cols <- unique(c(id_col, group_cols))

  distinct_values <- unique(dt[, c(by_cols, value_col), with = FALSE])[
    , .(n_values = .N),
    by = by_cols
  ]

  consistency <- distinct_values[
    , .(value_consistency = round(100 * mean(n_values == 1), digits)),
    by = group_cols
  ]

  if (!is.null(group_cols)) {
    data.table::setorderv(consistency, group_cols)
  }

  consistency[]
}

#' @rdname compute_value_consistency
#' @importFrom dplyr across all_of count distinct if_else summarise
#' @importFrom rlang check_dots_empty
#' @export
compute_value_consistency.tbl_dbi <- function(
  data,
  id_col,
  value_col,
  group_cols = NULL,
  digits = 2,
  ...
) {
  rlang::check_dots_empty()

  by_cols <- unique(c(id_col, group_cols))

  data |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(by_cols, value_col)))) |>
    dplyr::count(dplyr::across(dplyr::all_of(by_cols)), name = "n_values") |>
    dplyr::summarise(
      # whole digits, which SQL's ROUND() requires
      value_consistency = round(
        100 * mean(dplyr::if_else(n_values == 1, 1, 0), na.rm = TRUE),
        !!as.integer(digits)
      ),
      .by = dplyr::all_of(group_cols)
    )
}

#' Compute global consistency
#'
#' Averages record consistency and the value consistency of the columns in
#' `value_cols` into a single percentage for the whole table.
#'
#' @inheritParams compute_record_consistency
#' @param value_cols Character vector of columns whose values should stay the
#'   same for each identifier.
#'
#' @returns A number from 0 to 100.
#'
#' @details
#' The value consistencies of `value_cols` are averaged first, and that
#' average weighs as much as record consistency. Intermediate results are not
#' rounded, so rounding errors do not compound.
#'
#' This is a candidate to replace [govhr::compute_global_consistency()].
#' Unlike it, it works on database tables.
#'
#' @seealso [compute_record_consistency()] and
#'   [compute_value_consistency()], which it combines.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(
#'   personnel_id = c("a", "a", "b"),
#'   ref_date = as.Date(c("2020-01-01", "2021-01-01", "2020-01-01")),
#'   birth_date = as.Date(c("1980-01-01", "1981-01-01", "1990-01-01"))
#' )
#' compute_global_consistency(hr, "personnel_id", value_cols = "birth_date")
#' }
#'
#' @importFrom dplyr pull
#' @importFrom purrr map_dbl
#' @export
compute_global_consistency <- function(data, id_col, value_cols, digits = 2) {
  record_consistency <- compute_record_consistency(
    data,
    id_col = id_col,
    digits = 10
  ) |>
    dplyr::pull(.data[["record_consistency"]])

  value_consistency <- value_cols |>
    purrr::map_dbl(
      \(value_col) {
        compute_value_consistency(
          data,
          id_col = id_col,
          value_col = value_col,
          digits = 10
        ) |>
          dplyr::pull(.data[["value_consistency"]])
      }
    ) |>
    mean(na.rm = TRUE)

  round(mean(c(record_consistency, value_consistency), na.rm = TRUE), digits)
}

#' Plot consistency over time
#'
#' Draws record or value consistency for each reference date, one line per
#' group, from consistency already computed.
#'
#' @param data Data frame or lazy table (`tbl_dbi`) with `ref_date`, the
#'   column named in `group_col` and `record_consistency` or
#'   `value_consistency`, such as the output of [compute_record_consistency()]
#'   or [compute_value_consistency()] grouped by `group_col` and `ref_date`. A
#'   lazy table is brought into memory first.
#' @param group_col Character. Column to draw one line per group, or
#'   `"ref_date"` (default) for a single line.
#' @param type_plot Character. `"record"` (default) or `"value"`, the kind of
#'   consistency in `data`.
#' @param toggle_growth Logical. Show consistency as a baseline index, with
#'   each group's first date at 100. Default `FALSE`.
#'
#' @returns A ggplot2 object.
#'
#' @details
#' This is a candidate to replace [govhr::plot_consistency_trend()]. Unlike
#' it, it indexes the values when `toggle_growth` is `TRUE` rather than only
#' relabelling the axis, and drops the unused `id_col` and `value_col`
#' arguments.
#'
#' @seealso [compute_record_consistency()] and
#'   [compute_value_consistency()], which compute the consistency.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(
#'   personnel_id = c("a", "a", "b", "a"),
#'   ref_date = as.Date(c(rep("2020-01-01", 3), "2021-01-01"))
#' )
#' hr |>
#'   compute_record_consistency("personnel_id", group_cols = "ref_date") |>
#'   plot_consistency_trend()
#' }
#'
#' @importFrom dplyr collect rename
#' @importFrom govhr apply_baseline_index plot_trend
#' @export
plot_consistency_trend <- function(
  data,
  group_col = "ref_date",
  type_plot = c("record", "value"),
  toggle_growth = FALSE
) {
  type_plot <- match.arg(type_plot)
  consistency_col <- paste0(type_plot, "_consistency")

  # plot_trend() sizes its group colours from `data[[group_col]]`, which is
  # NULL on a lazy table, hence the collect(). apply_baseline_index() and
  # plot_trend() read the measure from `value`
  consistency <- data |>
    dplyr::collect() |>
    dplyr::rename(value = dplyr::all_of(consistency_col))

  if (toggle_growth) {
    consistency <- govhr::apply_baseline_index(
      consistency,
      group_col = group_col
    )
  }

  govhr::plot_trend(
    consistency,
    group_col = group_col,
    toggle_growth = toggle_growth,
    y_label = "Consistency"
  )
}

#' Plot value consistency by variable and group
#'
#' Draws a heatmap of value consistency, with groups across and variables
#' down, from consistency already computed.
#'
#' @param data Data frame with `variable`, `value_consistency` and the column
#'   named in `group_col`: one row per group and variable, such as
#'   [compute_value_consistency()] results for several value columns stacked
#'   with a `variable` column naming each.
#' @param group_col Character. Column whose values run across the heatmap.
#'   Default `"ref_date"`.
#'
#' @returns A plotly object.
#'
#' @details
#' This is a candidate to replace [govhr::plot_consistency_heatmap()]. Unlike
#' it, it takes computed consistency instead of computing it from raw data,
#' which also avoids picking value columns with `names()`, wrong on database
#' tables.
#'
#' @seealso [compute_value_consistency()], which computes the consistency.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(
#'   personnel_id = c("a", "a", "b"),
#'   ref_date = as.Date(c("2020-01-01", "2021-01-01", "2020-01-01")),
#'   birth_date = as.Date(c("1980-01-01", "1981-01-01", "1990-01-01"))
#' )
#' hr |>
#'   compute_value_consistency("personnel_id", "birth_date", group_cols = "ref_date") |>
#'   transform(variable = "birth_date") |>
#'   plot_consistency_heatmap()
#' }
#'
#' @importFrom dplyr mutate
#' @export
plot_consistency_heatmap <- function(data, group_col = "ref_date") {
  data |>
    dplyr::mutate(value_consistency = .data[["value_consistency"]] / 100) |>
    plot_quality_heatmap(
      group_col = group_col,
      value_col = "value_consistency",
      value_label = "Consistency"
    )
}

# a share heatmap with groups across, variables down and a red-to-green
# scale, shared by plot_coverage_heatmap() and plot_consistency_heatmap()
#' @importFrom plotly layout plot_ly
#' @importFrom stats as.formula
#' @keywords internal
#' @noRd
plot_quality_heatmap <- function(data, group_col, value_col, value_label) {
  plotly::plot_ly(
    data = data,
    x = stats::as.formula(paste0("~`", group_col, "`")),
    y = ~variable,
    z = stats::as.formula(paste0("~`", value_col, "`")),
    type = "heatmap",
    colorscale = list(c(0, "#d32f2f"), c(0.5, "#f9a825"), c(1, "#388e3c")),
    zmin = 0,
    zmax = 1,
    xgap = 2,
    ygap = 2,
    hovertemplate = paste0(
      "Group: %{x}<br>",
      "Variable: %{y}<br>",
      value_label, ": %{z:.0%}",
      "<extra></extra>"
    ),
    colorbar = list(title = value_label, tickformat = ".0%")
  ) |>
    plotly::layout(
      xaxis = list(title = "Group"),
      yaxis = list(title = "Variable")
    )
}
