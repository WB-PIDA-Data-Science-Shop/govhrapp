# ---- Data Summarization Helpers -----------------------------------------------

#' Compute Trend Summary
#'
#' Summarizes data over time by grouping variable, producing a tidy data frame
#' with `ref_date`, optional group column, and `value`. Used as the data source
#' for time trend plots.
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
#' @return A data frame with columns `ref_date`, optionally `group`, and `value`.
#'
#' @importFrom dplyr group_by across all_of summarise n
#' @importFrom govhr compute_fastsummary
#' @export
compute_trend_summary <- function(data, group, measure_col = NULL) {
  groups <- if (group == "ref_date") "ref_date" else c("ref_date", group)

  if (is.null(measure_col)) {
    data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(value = dplyr::n(), .groups = "drop")
  } else {
    data |>
      govhr::compute_fastsummary(cols = measure_col, fns = "sum", groups = groups)
  }
}

#' Apply Baseline Index to Trend Summary
#'
#' Rescales the `value` column so that the first observation equals 100,
#' producing a baseline index. When a grouping variable is present, the
#' rescaling is applied independently within each group.
#'
#' @param data A data frame with columns `ref_date` and `value`, as returned by
#'   [compute_trend_summary()].
#' @param group Character string naming the grouping column, or `"ref_date"` for
#'   no grouping.
#'
#' @return The input data frame with `value` rescaled to a baseline index.
#'
#' @importFrom dplyr arrange mutate group_by across all_of ungroup first
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
      dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
      dplyr::arrange(.data[["ref_date"]]) |>
      dplyr::mutate(
        value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
      ) |>
      dplyr::ungroup()
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
  data_latest <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
    dplyr::filter(.data[["ref_date"]] == max(.data[["ref_date"]])) |>
    dplyr::ungroup()

  if (is.null(measure_col)) {
    data_latest |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
      dplyr::summarise(value = dplyr::n(), .groups = "drop")
  } else {
    data_latest |>
      govhr::compute_fastsummary(cols = measure_col, fns = "sum", groups = group)
  }
}

#' Compute Growth Rate Summary
#'
#' Filters to the first and last reference date within each group and computes
#' the percentage change from first to last. Used as the data source for
#' growth-rate bar charts.
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
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
    dplyr::filter(
      .data[["ref_date"]] %in% c(max(.data[["ref_date"]]), min(.data[["ref_date"]]))
    ) |>
    dplyr::ungroup() |> 
    dplyr::arrange(.data[["ref_date"]])

  summarized <- if (is.null(measure_col)) {
    endpoints |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("ref_date", group)))) |>
      dplyr::summarise(value = dplyr::n(), .groups = "drop")
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
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
    dplyr::summarise(
      growth_rate = round(
        dplyr::last(.data[["value"]]) / dplyr::first(.data[["value"]]) - 1,
        3
      ) * 100,
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(.data[["growth_rate"]]))
}

#' Plot Time Trend
#'
#' Produces a ggplot2 line and point chart of `value` over `ref_date`. When a
#' grouping variable is present, each group receives its own line coloured with
#' an orange palette. When `toggle_growth` is `TRUE`, the y-axis is formatted
#' for a baseline index (first period = 100) with a reference line at 100;
#' otherwise raw values are shown with short-scale labels.
#'
#' @param data A data frame with columns `ref_date` and `value`, as returned by
#'   [compute_trend_summary()] and optionally [apply_baseline_index()].
#' @param group Character string naming the grouping column, or `"ref_date"` for
#'   no grouping.
#' @param toggle_growth Logical. If `TRUE`, format the y-axis as a baseline
#'   index and add a dashed reference line at 100. Default `FALSE`.
#' @param y_col Character string of the column to plot on the y-axis. Default `"value"`.
#' @param y_label Character string for the y-axis label used when
#'   `toggle_growth` is `FALSE`. Default `"Value"`.
#'
#' @return A ggplot2 object.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line xlab ylab scale_y_continuous geom_hline scale_color_manual
#' @importFrom dplyr n_distinct ungroup
#' @importFrom grDevices colorRampPalette
#' @importFrom scales label_number cut_short_scale
#' @export
plot_trend <- function(data, group, toggle_growth = FALSE, y_col = "value", y_label = "Value") {
  plot <- data |>
    dplyr::ungroup() |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["ref_date"]], y = .data[[y_col]])
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("Time")

  if (group != "ref_date") {
    n_groups <- dplyr::n_distinct(data[[group]], na.rm = TRUE)
    orange_palette <- grDevices::colorRampPalette(c("#C34729", "#F5C6A0"))(n_groups)
    plot <- plot +
      ggplot2::aes(
        color = .data[[group]],
        group = .data[[group]]
      ) +
      ggplot2::scale_color_manual(values = orange_palette)
  }

  if (toggle_growth) {
    plot <- plot +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(accuracy = 0.1)
      ) +
      ggplot2::ylab("Baseline index (first period = 100)") +
      ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "red3")
  } else {
    plot <- plot +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::ylab(y_label)
  }

  plot
}

#' Plot Horizontal Bar Chart of Totals by Group
#'
#' Produces a ggplot2 horizontal bar chart with groups ordered by `value`.
#' Missing values in either `value` or the group column are dropped. The x-axis
#' uses short-scale number formatting (e.g. 1K, 1M) and the y-axis uses
#' `guide_axis(n.dodge = 2)` to prevent overlapping labels.
#'
#' @param data A data frame with the grouping column and a `value` column, as
#'   returned by [compute_cross_section_summary()].
#' @param group Character string naming the grouping column.
#' @param x_label Character string for the x-axis label. Default `"Value"`.
#'
#' @return A ggplot2 object.
#'
#' @importFrom ggplot2 ggplot aes geom_col scale_x_continuous scale_y_discrete guide_axis labs
#' @importFrom dplyr filter
#' @importFrom stats reorder
#' @importFrom stringr str_wrap
#' @importFrom scales label_number cut_short_scale
#' @export
plot_bar_total <- function(data, group, x_label = "Value") {
  data |>
    dplyr::filter(
      !is.na(.data[["value"]]) & !is.na(.data[[group]])
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["value"]],
        y = stats::reorder(
          stringr::str_wrap(.data[[group]], width = 30),
          .data[["value"]]
        )
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::scale_y_discrete(guide = ggplot2::guide_axis(n.dodge = 2)) +
    ggplot2::labs(x = x_label, y = "")
}

#' Plot Horizontal Bar Chart of Growth Rates by Group
#'
#' Produces a ggplot2 horizontal bar chart with groups ordered by `growth_rate`.
#' A dashed vertical line is drawn at zero to distinguish positive from negative
#' growth. The x-axis uses short-scale number formatting and the y-axis uses
#' `guide_axis(n.dodge = 2)`.
#'
#' @param data A data frame with the grouping column and a `growth_rate` column,
#'   as returned by [compute_growth_summary()].
#' @param group Character string naming the grouping column.
#'
#' @return A ggplot2 object.
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_vline scale_x_continuous scale_y_discrete guide_axis labs
#' @importFrom stats reorder
#' @importFrom stringr str_wrap
#' @importFrom scales label_number cut_short_scale
#' @export
plot_bar_growth <- function(data, group) {
  data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["growth_rate"]],
        y = stats::reorder(
          stringr::str_wrap(.data[[group]], width = 30),
          .data[["growth_rate"]]
        )
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::geom_vline(
      xintercept = 0,
      linewidth = 1.25,
      linetype = "dashed",
      color = "#2958c3"
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::scale_y_discrete(guide = ggplot2::guide_axis(n.dodge = 2)) +
    ggplot2::labs(x = "Growth rate", y = "")
}

# ---- plot_segment -------------------------------------------------------------

#' Create a Segment Plot with Jittered Points
#'
#' Produces a ggplot2 visualization showing the range (min to max) and distribution
#' of values for a numeric variable across different groups. Groups are ordered by
#' their median values in descending order.
#'
#' @param .data A data frame containing the variables to plot.
#' @param col Character string specifying the name of the numeric column to plot
#'   on the x-axis.
#' @param group Character string specifying the name of the grouping column for
#'   the y-axis.
#'
#' @return A ggplot2 object displaying:
#'   \itemize{
#'     \item Grey horizontal segments showing the range (min to max) for each group
#'     \item Jittered points showing the distribution of individual observations
#'     \item Groups ordered by median value (highest to lowest, top to bottom)
#'   }
#'
#' @details
#' The function:
#' \itemize{
#'   \item Computes min, max, and median for each group
#'   \item Handles infinite values by converting them to NA
#'   \item Orders groups by median in descending order
#'   \item Uses hollow circles (shape = 1) for points with 70% transparency
#'   \item Applies minimal theme styling
#' }
#'
#' @examples
#' plot_segment(mtcars, col = "mpg", group = "cyl")
#'
#' @importFrom dplyr group_by summarise mutate arrange pull
#' @importFrom ggplot2 ggplot aes geom_segment geom_jitter scale_y_discrete labs
#' @importFrom tibble tibble
#' @importFrom rlang :=
#' 
#' @export
plot_segment <- function(.data, col, group) {
  df <- .data
  
  # Calculate summary statistics using .data[[]]
  summary_df <- df |>
    dplyr::filter(!is.na(.data[[col]]), !is.na(.data[[group]])) |>
    dplyr::group_by(.data[[group]]) |>
    dplyr::summarise(
      xmin = min(.data[[col]], na.rm = TRUE),
      xmax = max(.data[[col]], na.rm = TRUE),
      mean = mean(.data[[col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      xmin = ifelse(is.infinite(.data[["xmin"]]), NA_real_, .data[["xmin"]]),
      xmax = ifelse(is.infinite(.data[["xmax"]]), NA_real_, .data[["xmax"]])
    ) |>
    tibble::as_tibble()
  
  # Determine group ordering by median
  ordered_levels <- summary_df |>
    dplyr::arrange(dplyr::desc(.data[["mean"]])) |>
    dplyr::pull(.data[[group]]) |>
    as.character()
  
  # Apply factor ordering for plotting
  summary_df[[group]] <- factor(
    as.character(summary_df[[group]]), 
    levels = rev(ordered_levels)
  )
  
  plot_data <- df |>
    dplyr::mutate(
      !!group := factor(as.character(.data[[group]]), levels = rev(ordered_levels))
    )
  
  # Create the plot using .data[[]]
  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = summary_df,
      ggplot2::aes(
        x = .data[["xmin"]], 
        xend = .data[["xmax"]], 
        y = .data[[group]], 
        yend = .data[[group]]
      ),
      color = "grey70",
      linewidth = 1
    ) +
    ggplot2::geom_jitter(
      data = plot_data,
      ggplot2::aes(x = .data[[col]], y = .data[[group]]),
      height = 0.1,
      width = 0.1,
      alpha = 0.7,
      size = 2.5,
      shape = 1
    ) +
    ggplot2::scale_y_discrete() +
    ggplot2::labs(x = col, y = group)
}