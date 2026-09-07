# ---- Shared plot helpers ------------------------------------------------------

#' Orange Gradient Colour Scale for Grouped Series
#'
#' Builds the package's standard sequential orange scale, sized to the number of
#' distinct groups present in the data.
#'
#' @param values Vector of group values. Distinct non-missing values determine
#'   the number of colours.
#'
#' @return A ggplot2 manual colour scale.
#'
#' @importFrom dplyr n_distinct
#' @importFrom ggplot2 scale_color_manual
#' @importFrom grDevices colorRampPalette
#' @keywords internal
group_color_scale <- function(values) {
  n_groups <- dplyr::n_distinct(values, na.rm = TRUE)

  ggplot2::scale_color_manual(
    values = grDevices::colorRampPalette(c("#C34729", "#F5C6A0"))(n_groups)
  )
}

#' Plot Height for a Grouped Bar Chart
#'
#' Scales chart height with the number of bars so category labels stay legible.
#'
#' @param .data Data frame in which each row becomes one bar.
#'
#' @return Numeric height in pixels, never below 350.
#'
#' @keywords internal
scale_plot_height <- function(.data) {
  max(350, nrow(.data) * 35 + 100)
}

# ---- Data summarization helpers ----------------------------------------------

#' Compute Trend Summary
#'
#' Aggregates data over time into a tidy frame of `ref_date`, the optional
#' grouping column, and `value`. Counts rows when `measure_col` is `NULL`
#' (headcount) and sums the column otherwise (wage bill).
#'
#' @param .data Data frame containing at least a `ref_date` column.
#' @param group_col Character. Column to group by, or `"ref_date"` for no
#'   grouping.
#' @param measure_col Character. Numeric column to sum, or `NULL` to count rows.
#'
#' @return A data frame with `ref_date`, optionally `group_col`, and `value`.
#'
#' @importFrom dplyr across all_of
#' @importFrom govhr compute_fastsummary fastcount
#' @export
compute_trend_summary <- function(.data, group_col, measure_col = NULL) {
  groups <- if (group_col == "ref_date") "ref_date" else c("ref_date", group_col)

  if (is.null(measure_col)) {
    govhr::fastcount(.data, dplyr::across(dplyr::all_of(groups)), name = "value")
  } else {
    govhr::compute_fastsummary(
      .data,
      cols = measure_col,
      fns = "sum",
      groups = groups
    )
  }
}

#' Apply Baseline Index to a Trend Summary
#'
#' Rescales `value` so the earliest observation equals 100. When a grouping
#' column is supplied the rescaling is applied independently within each group.
#'
#' @param .data Data frame with `ref_date` and `value`, as returned by
#'   [compute_trend_summary()].
#' @param group_col Character. Column to group by, or `"ref_date"` for no
#'   grouping.
#'
#' @return The input data frame with `value` rescaled to a baseline index.
#'
#' @importFrom dplyr arrange mutate all_of first
#' @export
apply_baseline_index <- function(.data, group_col) {
  indexed <- dplyr::arrange(.data, .data[["ref_date"]])

  if (group_col == "ref_date") {
    dplyr::mutate(
      indexed,
      value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
    )
  } else {
    dplyr::mutate(
      indexed,
      value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100,
      .by = dplyr::all_of(group_col)
    )
  }
}

#' Compute Cross-Section Summary
#'
#' Keeps each group's latest reference date and aggregates it into a single
#' `value` per group. Counts rows when `measure_col` is `NULL` (headcount) and
#' sums the column otherwise (wage bill).
#'
#' @param .data Data frame containing `ref_date` and the grouping column.
#' @param group_col Character. Column to group by.
#' @param measure_col Character. Numeric column to sum, or `NULL` to count rows.
#'
#' @return A data frame with the grouping column and a `value` column.
#'
#' @importFrom dplyr all_of filter n summarise
#' @importFrom govhr compute_fastsummary
#' @export
compute_cross_section_summary <- function(.data, group_col, measure_col = NULL) {
  # only consider each group's latest reference date
  data_latest <- dplyr::filter(
    .data,
    .data[["ref_date"]] == max(.data[["ref_date"]]),
    .by = dplyr::all_of(group_col)
  )

  if (is.null(measure_col)) {
    dplyr::summarise(
      data_latest,
      value = dplyr::n(),
      .by = dplyr::all_of(group_col)
    )
  } else {
    govhr::compute_fastsummary(
      data_latest,
      cols = measure_col,
      fns = "sum",
      groups = group_col
    )
  }
}

#' Compute Growth Rate Summary
#'
#' Keeps each group's first and last reference date and computes the percentage
#' change between them. Counts rows when `measure_col` is `NULL` (headcount) and
#' sums the column otherwise (wage bill).
#'
#' @param .data Data frame containing `ref_date` and the grouping column.
#' @param group_col Character. Column to group by.
#' @param measure_col Character. Numeric column to sum, or `NULL` to count rows.
#'
#' @return A data frame with the grouping column and a `growth_rate` column, in
#'   percentage points (e.g. `12.5` for +12.5%).
#'
#' @importFrom dplyr all_of arrange filter first last n summarise
#' @importFrom govhr compute_fastsummary
#' @export
compute_growth_summary <- function(.data, group_col, measure_col = NULL) {
  labelled <- dplyr::filter(.data, !is.na(.data[[group_col]]))

  # aggregate every period once; picking endpoints out of the (small) aggregate
  # is cheaper than scanning the raw rows for each group's first and last date
  by_period <- if (is.null(measure_col)) {
    dplyr::summarise(
      labelled,
      value = dplyr::n(),
      .by = dplyr::all_of(c("ref_date", group_col))
    )
  } else {
    govhr::compute_fastsummary(
      labelled,
      cols = measure_col,
      fns = "sum",
      groups = c("ref_date", group_col)
    )
  }

  by_period |>
    dplyr::filter(
      .data[["ref_date"]] %in% range(.data[["ref_date"]]),
      .by = dplyr::all_of(group_col)
    ) |>
    dplyr::arrange(.data[["ref_date"]]) |>
    dplyr::summarise(
      growth_rate = round(
        dplyr::last(.data[["value"]]) / dplyr::first(.data[["value"]]) - 1,
        3
      ) *
        100,
      .by = dplyr::all_of(group_col)
    ) |>
    dplyr::filter(!is.na(.data[["growth_rate"]]))
}

# ---- Plots -------------------------------------------------------------------

#' Plot Time Trend
#'
#' Draws a line-and-point chart of the measure over `ref_date`, one coloured
#' series per group. When `toggle_growth` is `TRUE` the y-axis is formatted as a
#' baseline index with a dashed reference line at 100.
#'
#' @param .data Data frame with `ref_date` and the y-axis column, as returned by
#'   [compute_trend_summary()] and optionally [apply_baseline_index()].
#' @param group_col Character. Column to group by, or `"ref_date"` for no
#'   grouping.
#' @param toggle_growth Logical. Format the y-axis as a baseline index. Default
#'   `FALSE`.
#' @param y_col Character. Column to plot on the y-axis. Default `"value"`.
#' @param y_label Character. y-axis label, used when `toggle_growth` is `FALSE`.
#'   Default `"Value"`.
#'
#' @return A ggplot2 object.
#'
#' @importFrom ggplot2 aes geom_hline geom_line geom_point ggplot scale_y_continuous xlab ylab
#' @importFrom scales cut_short_scale label_number
#' @export
plot_trend <- function(
  .data,
  group_col,
  toggle_growth = FALSE,
  y_col = "value",
  y_label = "Value"
) {
  plot <- .data |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["ref_date"]], y = .data[[y_col]])
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("Time")

  if (group_col != "ref_date") {
    plot <- plot +
      ggplot2::aes(
        color = .data[[group_col]],
        group = .data[[group_col]]
      ) +
      group_color_scale(.data[[group_col]])
  }

  if (toggle_growth) {
    plot +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(accuracy = 0.1)
      ) +
      ggplot2::ylab("Baseline index (first period = 100)") +
      ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "red3")
  } else {
    plot +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::ylab(y_label)
  }
}

#' Plot Totals by Group
#'
#' Draws a horizontal bar chart with groups ordered by the plotted value. Rows
#' missing either the value or the group label are dropped.
#'
#' @param .data Data frame with the grouping column and the x-axis column, as
#'   returned by [compute_cross_section_summary()].
#' @param group_col Character. Column to group by.
#' @param x_col Character. Column to plot on the x-axis. Default `"value"`.
#' @param x_label Character. x-axis label. Default `"Value"`.
#'
#' @return A ggplot2 object.
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes geom_col ggplot guide_axis labs scale_x_continuous scale_y_discrete
#' @importFrom scales cut_short_scale label_number
#' @importFrom stats reorder
#' @importFrom stringr str_wrap
#' @export
plot_bar_total <- function(.data, group_col, x_col = "value", x_label = "Value") {
  .data |>
    dplyr::filter(
      !is.na(.data[[x_col]]) & !is.na(.data[[group_col]])
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[[x_col]],
        y = stats::reorder(
          stringr::str_wrap(.data[[group_col]], width = 30),
          .data[[x_col]]
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

#' Plot Growth Rates by Group
#'
#' Draws a horizontal bar chart with groups ordered by `growth_rate`, with a
#' dashed reference line at zero separating growth from decline.
#'
#' @param .data Data frame with the grouping column and a `growth_rate` column,
#'   as returned by [compute_growth_summary()].
#' @param group_col Character. Column to group by.
#'
#' @return A ggplot2 object.
#'
#' @importFrom ggplot2 aes geom_col geom_vline ggplot guide_axis labs scale_x_continuous scale_y_discrete
#' @importFrom scales cut_short_scale label_number
#' @importFrom stats reorder
#' @importFrom stringr str_wrap
#' @export
plot_bar_growth <- function(.data, group_col) {
  .data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["growth_rate"]],
        y = stats::reorder(
          stringr::str_wrap(.data[[group_col]], width = 30),
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

#' Plot Value Ranges by Group
#'
#' Draws a grey range segment from each group's minimum to its maximum, overlaid
#' with jittered observations. Groups are ordered by mean value, highest at the
#' top.
#'
#' @param .data Data frame containing the variables to plot.
#' @param measure_col Character. Numeric column to plot on the x-axis.
#' @param group_col Character. Column to group by, plotted on the y-axis.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' plot_segment(mtcars, measure_col = "mpg", group_col = "cyl")
#'
#' @importFrom dplyr arrange desc mutate pull summarise
#' @importFrom ggplot2 aes geom_jitter geom_segment ggplot labs scale_y_discrete
#' @importFrom rlang :=
#' @importFrom stats na.omit
#' @importFrom tibble as_tibble
#' @export
plot_segment <- function(.data, measure_col, group_col) {
  summary_df <- .data |>
    dplyr::summarise(
      xmin = min(.data[[measure_col]], na.rm = TRUE),
      xmax = max(.data[[measure_col]], na.rm = TRUE),
      mean = mean(.data[[measure_col]], na.rm = TRUE),
      .by = .data[[group_col]]
    ) |>
    # drop groups with any missing component
    stats::na.omit() |>
    dplyr::mutate(
      xmin = ifelse(is.infinite(.data[["xmin"]]), NA_real_, .data[["xmin"]]),
      xmax = ifelse(is.infinite(.data[["xmax"]]), NA_real_, .data[["xmax"]])
    ) |>
    tibble::as_tibble()

  ordered_levels <- summary_df |>
    dplyr::arrange(dplyr::desc(.data[["mean"]])) |>
    dplyr::pull(.data[[group_col]]) |>
    as.character()

  summary_df[[group_col]] <- factor(
    as.character(summary_df[[group_col]]),
    levels = rev(ordered_levels)
  )

  plot_data <- .data |>
    dplyr::mutate(
      !!group_col := factor(
        as.character(.data[[group_col]]),
        levels = rev(ordered_levels)
      )
    )

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = summary_df,
      ggplot2::aes(
        x = .data[["xmin"]],
        xend = .data[["xmax"]],
        y = .data[[group_col]],
        yend = .data[[group_col]]
      ),
      color = "grey70",
      linewidth = 1
    ) +
    ggplot2::geom_jitter(
      data = plot_data,
      ggplot2::aes(x = .data[[measure_col]], y = .data[[group_col]]),
      height = 0.1,
      width = 0.1,
      alpha = 0.7,
      size = 2.5,
      shape = 1
    ) +
    ggplot2::scale_y_discrete() +
    ggplot2::labs(x = measure_col, y = group_col)
}

#' Plot Personnel Movement Over Time
#'
#' Draws movement counts or rates over `ref_date`, one coloured series per
#' group. Turnover is drawn against a dashed replacement-rate reference line
#' at 1.
#'
#' @param .data Data frame with `ref_date`, `indicator`, and optionally the
#'   grouping column.
#' @param movement_type Character. One of `"hire"`, `"fire"`, `"retirement"` or
#'   `"turnover"`.
#' @param measurement_type Character. Either `"count"` or `"rate"`.
#' @param group_col Character. Column to group by, or `"ref_date"` for no
#'   grouping.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 aes annotate geom_hline geom_line geom_point ggplot labs scale_y_continuous
#' @importFrom scales label_number percent_format
#' @export
plot_movement <- function(.data, movement_type, measurement_type, group_col) {
  plot <- .data |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["ref_date"]], y = .data[["indicator"]])
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Time",
      y = ifelse(measurement_type == "rate", "Share", "Count")
    )

  if (group_col != "ref_date") {
    plot <- plot +
      ggplot2::aes(
        color = .data[[group_col]],
        group = .data[[group_col]]
      ) +
      group_color_scale(.data[[group_col]])
  }

  is_flow <- movement_type %in% c("hire", "fire", "retirement")

  if (is_flow && measurement_type == "rate") {
    plot <- plot +
      ggplot2::scale_y_continuous(labels = scales::percent_format())
  } else if (movement_type == "turnover") {
    ref_dates <- as.Date(.data[["ref_date"]])

    plot <- plot +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(accuracy = 0.1)
      ) +
      ggplot2::geom_hline(
        yintercept = 1,
        linetype = "dashed",
        color = "#004181"
      ) +
      ggplot2::annotate(
        "text",
        x = max(ref_dates) - (max(ref_dates) - min(ref_dates)) * 0.05,
        y = 1.15,
        label = "Replacement rate = 1",
        color = "#004181"
      ) +
      ggplot2::labs(y = "Replacement rate")
  }

  plot
}

#' Plot Mean Wage by Decile
#'
#' Draws a bar chart of the mean measure within each decile, faceted by group
#' when one is supplied.
#'
#' @param .data Data frame with `decile` and `mean_value`, as returned by
#'   [compute_decile()].
#' @param group_col Character. Column to facet by, or `"ref_date"` for a single
#'   panel.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 aes facet_wrap geom_col ggplot label_wrap_gen labs scale_x_continuous vars
#' @keywords internal
plot_decile <- function(.data, group_col) {
  plot <- .data |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["decile"]], y = .data[["mean_value"]])
    ) +
    ggplot2::geom_col(fill = "#C34729") +
    ggplot2::labs(x = "Decile", y = "Median by Decile") +
    ggplot2::scale_x_continuous(breaks = 1:10, labels = 1:10)

  if (group_col != "ref_date") {
    plot <- plot +
      ggplot2::facet_wrap(
        ggplot2::vars(.data[[group_col]]),
        labeller = ggplot2::label_wrap_gen(width = 20)
      )
  }

  plot
}

#' Plot Wage Distribution
#'
#' Draws binned wage shares as a histogram or as a cumulative distribution,
#' faceted by group when one is supplied.
#'
#' @param .data Data frame with `bin`, `pct` and `cum_pct`, as returned by
#'   [compute_percentile()].
#' @param plot_type Character. Either `"histogram"` or `"cumulative"`. Default
#'   `"histogram"`.
#' @param group_col Character. Column to facet by, or `NULL`/`"ref_date"` for a
#'   single panel.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 aes facet_wrap geom_col ggplot label_wrap_gen labs scale_y_continuous vars
#' @importFrom scales label_percent
#' @keywords internal
plot_histogram <- function(.data, plot_type = "histogram", group_col = NULL) {
  plot_type <- match.arg(plot_type, c("histogram", "cumulative"))

  group_col <- if (is.null(group_col)) "ref_date" else group_col

  y_col <- switch(plot_type, histogram = "pct", cumulative = "cum_pct")

  plot <- .data |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["bin"]], y = .data[[y_col]])
    ) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::labs(x = "", y = "Percentage Share")

  if (group_col != "ref_date") {
    plot <- plot +
      ggplot2::facet_wrap(
        ggplot2::vars(.data[[group_col]]),
        labeller = ggplot2::label_wrap_gen(width = 20)
      )
  }

  plot
}

#' Plot Wage Compression Ratio
#'
#' Draws the median wage per group as a point, spanned by a line range between
#' the lower and upper percentile bounds.
#'
#' @param .data Data frame with `percentile_50`, `percentile_lower` and
#'   `percentile_upper`, as returned by [govhr::compute_compression_ratio()].
#' @param group_col Character. Column to group by, or `NULL`/`"ref_date"` for no
#'   grouping.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 aes geom_linerange geom_point ggplot labs
#' @keywords internal
plot_compression_ratio <- function(.data, group_col) {
  group_col <- if (is.null(group_col)) "ref_date" else group_col

  plot <- .data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["percentile_50"]],
        y = .data[[group_col]],
        xmin = .data[["percentile_lower"]],
        xmax = .data[["percentile_upper"]]
      )
    ) +
    ggplot2::geom_point(size = 3, color = "#C34729") +
    ggplot2::geom_linerange(color = "#C34729") +
    ggplot2::labs(
      x = "Wage Compression Ratio (10th to 90th Percentile)",
      y = ""
    )

  if (group_col != "ref_date") {
    plot <- plot +
      ggplot2::aes(
        color = .data[[group_col]],
        group = .data[[group_col]]
      ) +
      group_color_scale(.data[[group_col]])
  }

  plot
}

#' Plot Movement Cost by Group
#'
#' Draws a horizontal bar chart of labour movement cost per group.
#'
#' @param .data Data frame with `movement_cost`, as returned by
#'   [govhr::compute_movement_cost()].
#' @param group_col Character. Column to group by, or `NULL`/`"ref_date"` for no
#'   grouping.
#'
#' @return A plotly object.
#'
#' @importFrom ggplot2 aes geom_col ggplot labs
#' @importFrom plotly ggplotly
#' @keywords internal
plot_movement_cost <- function(.data, group_col) {
  group_col <- if (is.null(group_col)) "ref_date" else group_col

  plot <- .data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["movement_cost"]],
        y = .data[[group_col]]
      )
    ) +
    ggplot2::geom_col(fill = "#C34729") +
    ggplot2::labs(x = "Movement Cost", y = "")

  if (group_col != "ref_date") {
    plot <- plot +
      ggplot2::aes(
        color = .data[[group_col]],
        group = .data[[group_col]]
      ) +
      group_color_scale(.data[[group_col]])
  }

  plotly::ggplotly(plot)
}

#' Plot Transfer Heatmap
#'
#' Draws transfers between groups as a heatmap, origin groups on the y-axis and
#' destination groups on the x-axis.
#'
#' @param .data Data frame with `from`, `to` and `transfer` columns.
#'
#' @return A plotly object.
#'
#' @importFrom plotly layout plot_ly
#' @importFrom stats median
#' @keywords internal
plot_transfer_heatmap <- function(.data) {
  transfer <- .data[["transfer"]]

  plotly::plot_ly(
    data = .data,
    x = ~ .data[["to"]],
    y = ~ .data[["from"]],
    z = ~ .data[["transfer"]],
    type = "heatmap",
    colorscale = list(
      c(min(transfer, na.rm = TRUE), "#d32f2f"),
      c(stats::median(transfer, na.rm = TRUE), "#f9a825"),
      c(max(transfer, na.rm = TRUE), "#388e3c")
    ),
    zmin = min(transfer, na.rm = TRUE),
    zmax = max(transfer, na.rm = TRUE),
    xgap = 2,
    ygap = 2,
    hovertemplate = paste0(
      "Group (to): %{x}<br>",
      "Group (from): %{y}<br>",
      "Transfers: %{z}",
      "<extra></extra>"
    ),
    colorbar = list(title = "Transfers")
  ) |>
    plotly::layout(
      xaxis = list(title = "Group (to)"),
      yaxis = list(title = "Group (from)")
    )
}

#' Plot Transition Network
#'
#' Draws career transitions as a directed graph, with edge width proportional to
#' the number of transitions and node size to degree centrality. Networks of ten
#' or more nodes are labelled by index rather than by name.
#'
#' @param .data Data frame with `from` and `to` columns, as returned by
#'   [detect_career_transition()].
#'
#' @return A ggiraph girafe object.
#'
#' @importFrom dplyr pull row_number
#' @importFrom ggplot2 aes scale_color_manual scale_size_identity theme theme_void
#' @importFrom govhr fastcount
#' @importFrom grDevices colorRampPalette
#' @importFrom tidygraph as_tbl_graph
#' @keywords internal
plot_transition_network <- function(.data) {
  edges <- govhr::fastcount(.data, .data[["from"]], .data[["to"]], name = "weight")

  graph_data <- tidygraph::as_tbl_graph(edges, directed = TRUE)

  n_nodes <- igraph::gorder(graph_data)
  many_nodes <- n_nodes >= 10
  orange_palette <- grDevices::colorRampPalette(c("#C34729", "#F5C6A0"))(n_nodes)

  graph_data <- graph_data |>
    tidygraph::activate(nodes) |>
    tidygraph::mutate(
      node_id = as.character(dplyr::row_number()),
      node_id = factor(
        node_id,
        levels = as.character(sort(as.integer(node_id)))
      ),
      label = if (many_nodes) node_id else name,
      degree = tidygraph::centrality_degree(mode = "all")
    )

  point_size <- scales::rescale(
    graph_data |> tidygraph::activate(nodes) |> dplyr::pull(degree),
    to = if (many_nodes) c(6, 14) else c(20, 30)
  )

  plot <- ggraph::ggraph(graph_data, layout = "stress") +
    ggraph::geom_edge_arc(
      ggplot2::aes(edge_width = weight, edge_alpha = 0.5),
      color = "#4a5568",
      arrow = grid::arrow(length = grid::unit(3, "mm"), type = "closed"),
      end_cap = ggraph::circle(4, "mm"),
      start_cap = ggraph::circle(4, "mm")
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(
        size = point_size,
        color = if (many_nodes) node_id else "#C34729"
      )
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(label = if (many_nodes) node_id else name),
      color = if (many_nodes) "white" else "#2d224e",
      size = if (many_nodes) 3 else 8,
      fontface = "bold"
    ) +
    # invisible layer carrying the hover tooltips
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        x = x,
        y = y,
        size = point_size,
        tooltip = name,
        data_id = node_id
      ),
      alpha = 0.01
    ) +
    ggraph::scale_edge_width_continuous(range = c(0.2, 3), guide = "none") +
    ggraph::scale_edge_alpha_identity(guide = "none") +
    ggplot2::scale_size_identity(guide = "none") +
    ggplot2::scale_color_manual(values = orange_palette, guide = "none") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  ggiraph::girafe(
    ggobj = plot,
    options = list(
      ggiraph::opts_hover(css = "stroke:#2d224e;stroke-width:2px;")
    )
  )
}
