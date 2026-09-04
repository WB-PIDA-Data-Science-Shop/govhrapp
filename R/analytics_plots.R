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
#' @importFrom dplyr group_by across all_of
#' @importFrom govhr compute_fastsummary fastcount
#' @export
compute_trend_summary <- function(data, group, measure_col = NULL) {
  groups <- if (group == "ref_date") "ref_date" else c("ref_date", group)

  if (is.null(measure_col)) {
    data |>
      govhr::fastcount(
        dplyr::across(
          dplyr::all_of(groups)
        ),
        name = "value"
      )
  } else {
    data |>
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
#' rescaling is applied independently within each group.
#'
#' @param data A data frame with columns `ref_date` and `value`, as returned by
#'   [compute_trend_summary()].
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
plot_trend <- function(
  data,
  group,
  toggle_growth = FALSE,
  y_col = "value",
  y_label = "Value"
) {
  plot <- data |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["ref_date"]], y = .data[[y_col]])
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("Time")

  if (group != "ref_date") {
    n_groups <- dplyr::n_distinct(data[[group]], na.rm = TRUE)
    orange_palette <- grDevices::colorRampPalette(c("#C34729", "#F5C6A0"))(
      n_groups
    )
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
#' @param x_col Character string of the column to plot on the x-axis. Default `"value"`.
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
plot_bar_total <- function(data, group, x_col = "value", x_label = "Value") {
  data |>
    dplyr::filter(
      !is.na(.data[[x_col]]) & !is.na(.data[[group]])
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[[x_col]],
        y = stats::reorder(
          stringr::str_wrap(.data[[group]], width = 30),
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
    dplyr::summarise(
      xmin = min(.data[[col]], na.rm = TRUE),
      xmax = max(.data[[col]], na.rm = TRUE),
      mean = mean(.data[[col]], na.rm = TRUE),
      .by = .data[[group]]
    ) |>
    # drop if any components are missing for a group
    na.omit() |>
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
      !!group := factor(
        as.character(.data[[group]]),
        levels = rev(ordered_levels)
      )
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

#' Plot Personnel Movement Over Time
#' 
#' @param .data A data frame containing the movement data with columns `ref_date`, `indicator`, and optionally a grouping column.
#' @param movement_type A character string indicating the type of movement: "hire", "fire", or "turnover".
#' @param measurement_type A character string indicating the measurement type: "count" or "rate".
#' @param group_cols A character string indicating the grouping column, or "ref_date" for no grouping.
#' 
#' @return A plotly object representing the personnel movement over time.
#' 
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs scale_y_continuous
#' @importFrom dplyr n_distinct
#' @importFrom grDevices colorRampPalette
#' @importFrom plotly ggplotly
#' 
#' @export
plot_movement <- function(.data, movement_type, measurement_type, group_cols) {
  plot <- .data |>
    ggplot(
      aes(.data[["ref_date"]], .data[["indicator"]])
    ) +
    geom_point() +
    geom_line() +
    labs(
      x = "Time",
      y = ifelse(measurement_type == "rate", "Share", "Count")
    )

  if (group_cols != "ref_date") {
    n_groups <- dplyr::n_distinct(
      .data[[group_cols]],
      na.rm = TRUE
    )
    orange_palette <- colorRampPalette(c("#C34729", "#F5C6A0"))(n_groups)
    plot <- plot +
      aes(
        color = .data[[group_cols]],
        group = .data[[group_cols]]
      ) +
      ggplot2::scale_color_manual(values = orange_palette)
  }

  if (movement_type %in% c("hire", "fire", "retirement") & measurement_type == "rate") {
    plot <- plot +
      scale_y_continuous(
        labels = scales::percent_format()
      )
  } else if (movement_type == "turnover") {
    plot <- plot +
      scale_y_continuous(
        labels = scales::label_number(accuracy = 0.1)
      ) +
      geom_hline(
        yintercept = 1,
        linetype = "dashed",
        color = "#004181"
      ) +
      ggplot2::annotate(
        "text",
        x = as.Date(max(.data[["ref_date"]])) -
          (as.Date(max(.data[["ref_date"]])) -
            as.Date(min(.data[["ref_date"]]))) *
            0.05,
        y = 1.15,
        label = "Replacement rate = 1",
        color = "#004181"
      ) +
      labs(
        y = "Replacement rate"
      )
  }

  plotly::ggplotly(plot)
}

plot_decile <- function(.data, group_cols){
  plot <- .data |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["decile"]], y = .data[["mean_value"]])
    ) +
    ggplot2::geom_col(
      fill = "#C34729"
    ) +
    ggplot2::labs(
      x = "Decile",
      y = "Median by Decile"
    ) +
    ggplot2::scale_x_continuous(
      breaks = 1:10,
      labels = 1:10
    )

  if (group_cols != "ref_date") {
    plot <- plot +
      facet_wrap(
        ggplot2::vars(.data[[group_cols]]),
        scales = "fixed"
      )
  }

  # if group are present, facet the plot by group
  if (group_cols != "ref_date") {
    plot <- plot +
      ggplot2::facet_wrap(
        ggplot2::vars(.data[[group_cols]]),
        labeller = ggplot2::label_wrap_gen(width = 20)
      )
  }

  plotly::ggplotly(plot)
}

#' Plot Density as Percentage Share
#'
#' @param .data A data frame.
#' @param plot_type A character string indicating the type of plot: "histogram" or "cumulative".
#' @param group_col The column name to group by.
#'
#' @importFrom ggplot2 ggplot aes geom_density scale_y_continuous labs theme_minimal
#' @importFrom plotly ggplotly
#' @importFrom grDevices colorRampPalette
#' 
#' @return A plotly object.
plot_histogram <- function(.data, plot_type = "histogram", group_col = NULL) {
  plot_type <- match.arg(plot_type, c("histogram", "cumulative"))

  y_var <- switch(
    plot_type,
    histogram = "pct",
    cumulative = "cum_pct"
  )

  plot <- .data |> 
    ggplot2::ggplot(ggplot2::aes(x = bin, y = .data[[y_var]])) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::labs(x = "", y = "Percentage Share")

  if (!is.null(group_col)) {
    plot <- plot +
      ggplot2::facet_wrap(
        ggplot2::vars(.data[[group_col]]),
        labeller = ggplot2::label_wrap_gen(width = 20)
      )
  }

  plotly::ggplotly(plot)
}

plot_compression_ratio <- function(.data, group_cols){
  group_cols <- if (is.null(group_cols)) "ref_date" else group_cols

  # plot as a line range between percentile_10 and percentile_90, with a point at percentile_50
  # and the y-axis is the group_cols, and the x-axis is the percentile values
  plot <- .data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["percentile_50"]],
        y = .data[[group_cols]],
        xmin = .data[["percentile_lower"]],
        xmax = .data[["percentile_upper"]]
      )
    ) +
    ggplot2::geom_point(
      size = 3,
      color = "#C34729"
    ) +
    ggplot2::geom_linerange(
      color = "#C34729"
    ) +
    ggplot2::labs(
      x = "Wage Compression Ratio (10th to 90th Percentile)",
      y = ""
    )
  
    if (group_cols != "ref_date") {
    n_groups <- dplyr::n_distinct(
      .data[[group_cols]],
      na.rm = TRUE
    )
    orange_palette <- colorRampPalette(c("#C34729", "#F5C6A0"))(n_groups)
    plot <- plot +
      aes(
        color = .data[[group_cols]],
        group = .data[[group_cols]]
      ) +
      ggplot2::scale_color_manual(values = orange_palette)
  }

  plotly::ggplotly(plot)
}

plot_movement_cost <- function(.data, group_cols){
  group_cols <- if (is.null(group_cols)) "ref_date" else group_cols

  plot <- .data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["movement_cost"]],
        y = .data[[group_cols]]
      )
    ) +
    ggplot2::geom_col(
      fill = "#C34729"
    ) +
    ggplot2::labs(
      x = "Movement Cost",
      y = ""
    )

  if (group_cols != "ref_date") {
    n_groups <- dplyr::n_distinct(
      .data[[group_cols]],
      na.rm = TRUE
    )
    orange_palette <- colorRampPalette(c("#C34729", "#F5C6A0"))(n_groups)
    plot <- plot +
      aes(
        color = .data[[group_cols]],
        group = .data[[group_cols]]
      ) +
      ggplot2::scale_color_manual(values = orange_palette)
  }

  plotly::ggplotly(plot)
}


#' Plot Transfer Heatmap
#'
#' @param .data A data frame.
#'
#' @importFrom plotly plot_ly
#' @importFrom dplyr across everything summarise mutate
#' @importFrom tidyr pivot_longer
#' @importFrom scales label_percent
#'
#' @return A ggplot2 object representing a heatmap of transfer values between groups
plot_transfer_heatmap <- function(.data) {
  # plot heatmap
  plotly::plot_ly(
    data = .data,
    x = ~ .data[["to"]],
    y = ~ .data[["from"]],
    z = ~ .data[["transfer"]],
    type = "heatmap",
    colorscale = list(
      c(min(.data[["transfer"]], na.rm = TRUE), "#d32f2f"),
      c(median(.data[["transfer"]], na.rm = TRUE), "#f9a825"),
      c(max(.data[["transfer"]], na.rm = TRUE), "#388e3c")
    ),
    zmin = min(.data[["transfer"]], na.rm = TRUE),
    zmax = max(.data[["transfer"]], na.rm = TRUE),
    xgap = 2,
    ygap = 2,
    hovertemplate = paste0(
      "Group (to): %{x}<br>",
      "Group (from): %{y}<br>",
      "Transfers: %{z}",
      "<extra></extra>"
    ),
    colorbar = list(
      title = "Transfers"
    )
  ) |>
    plotly::layout(
      xaxis = list(title = "Group (to)"),
      yaxis = list(title = "Group (from)")
    )
}

#' Plot Transfer Network
#' 
#' @param .data A data frame containing the transfer data with columns `from`, `to`, and `weight`.
#' 
#' @importFrom tidygraph as_tbl_graph
#' @importFrom ggraph ggraph geom_edge_fan geom_node_point geom_node_text scale_edge_width_continuous scale_edge_alpha_identity
#' @importFrom ggplot2 aes stage after_stat theme_void
#' 
#' @return A ggplot2 object representing a transfer network.
plot_transfer_network <- function(.data) {
  .data <- .data |>
    govhr::fastcount(.data[["from"]], .data[["to"]], name = "weight")

  # Convert to tidygraph object
  graph_data <- tidygraph::as_tbl_graph(.data, directed = TRUE)

  # Generate the graph plot
  plot <- ggraph::ggraph(graph_data, layout = "centrality") +
    ggraph::geom_edge_arc(
      ggplot2::aes(
        edge_width = weight,
        edge_alpha = 0.5
      ),
      color = "#4a5568"
    ) +
    # annote inflows and outflows
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "#2d224e"
    ) +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0.1,
      label = "Outflows",
      color = "#2d224e",
      size = 6,
      fontface = "bold"
    ) +
      ggplot2::annotate(
        "text",
        x = 0,
        y = -0.1,
        label = "Inflows",
        color = "#2d224e",
        size = 6,
        fontface = "bold"   
    ) +
    ggraph::geom_node_point(size = 30, color = "#2d224e") +
    ggraph::geom_node_text(aes(label = name), color = "white", size = 10, fontface = "bold") +
    ggraph::scale_edge_width_continuous(range = c(0.2, 5), guide = "none") +
    ggraph::scale_edge_alpha_identity(guide = "none") +
    ggplot2::theme_void()

  plot
}

#' Plot Transfer Network (plotly implementation)
#'
#' @param .data A data frame containing the transfer data with columns `from` and `to`.
#'
#' @importFrom tidygraph as_tbl_graph activate as_tibble
#' @importFrom igraph as.igraph V as_data_frame
#' @importFrom plotly plot_ly add_trace layout config
#' @importFrom scales rescale
#' @importFrom dplyr mutate
#'
#' @return A plotly object representing a transfer network arc diagram.
plotly_transfer_network <- function(.data) {
  .data <- .data |>
    govhr::fastcount(.data[["from"]], .data[["to"]], name = "weight")

  ig <- tidygraph::as_tbl_graph(.data, directed = TRUE) |>
    igraph::as.igraph()

  # --- Linear layout ----------------------------------------------------------
  node_names <- igraph::V(ig)$name
  n_nodes    <- length(node_names)

  nodes <- data.frame(
    name  = node_names,
    x     = seq(0, 1, length.out = n_nodes),
    y     = 0,
    order = seq_len(n_nodes),
    stringsAsFactors = FALSE
  )

  # --- Edge data --------------------------------------------------------------
  edges_df <- igraph::as_data_frame(ig, what = "edges")

  edges_df$x0         <- nodes$x[match(edges_df$from, nodes$name)]
  edges_df$x1         <- nodes$x[match(edges_df$to,   nodes$name)]
  edges_df$from_order <- nodes$order[match(edges_df$from, nodes$name)]
  edges_df$to_order   <- nodes$order[match(edges_df$to,   nodes$name)]

  # Matches fold = TRUE logic: from > to (internal index) goes below
  edges_df$is_regression <- edges_df$from_order > edges_df$to_order
  edges_df$direction     <- ifelse(edges_df$is_regression, -1, 1)
  edges_df$width         <- scales::rescale(edges_df$weight, to = c(0.5, 6))

  # --- Arc Bézier helper ------------------------------------------------------
  make_arc <- function(x0, x1, direction = 1, arc_scale = 0.5, n = 60) {
    cx <- (x0 + x1) / 2
    cy <- direction * abs(x1 - x0) * arc_scale

    t <- seq(0, 1, length.out = n)
    data.frame(
      x = c((1 - t)^2 * x0 + 2 * (1 - t) * t * cx + t^2 * x1, NA_real_),
      y = c((1 - t)^2 * 0  + 2 * (1 - t) * t * cy + t^2 * 0,  NA_real_)
    )
  }

  # --- Build figure -----------------------------------------------------------
  fig <- plotly::plot_ly()

  for (i in seq_len(nrow(edges_df))) {
    e   <- edges_df[i, ]
    pts <- make_arc(e$x0, e$x1, direction = e$direction)

    fig <- plotly::add_trace(
      fig,
      type       = "scatter",
      mode       = "lines",
      x          = pts$x,
      y          = pts$y,
      line       = list(color = "rgba(74,85,104,0.5)", width = e$width, shape = "spline"),
      hoverinfo  = "none",
      showlegend = FALSE
    )
  }

  # Node markers + labels
  fig <- plotly::add_trace(
    fig,
    type         = "scatter",
    mode         = "markers+text",
    x            = nodes$x,
    y            = nodes$y,
    text         = nodes$name,
    textposition = "middle center",
    textfont     = list(color = "white", size = 13, family = "Arial Black"),
    marker       = list(size = 30, color = "#2d224e", line = list(width = 0)),
    hovertext    = nodes$name,
    hoverinfo    = "text",
    showlegend   = FALSE
  )

  max_span <- max(abs(edges_df$x1 - edges_df$x0)) * 0.5 + 0.15

  fig |>
    plotly::layout(
      xaxis = list(visible = FALSE, range = c(-0.15, 1.15)),
      yaxis = list(visible = FALSE, range = c(-max_span, max_span)),
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      margin = list(l = 10, r = 10, t = 30, b = 30),
      # Dashed separator line
      shapes = list(
        list(
          type    = "line",
          x0      = 0, x1 = 1,
          y0      = 0, y1 = 0,
          xref    = "x", yref = "y",
          line    = list(color = "#2d224e", dash = "dash", width = 1),
          layer = "below"
        )
      ),
      # Outflows / Inflows annotations
      annotations = list(
        list(
          x         = 0,
          y         = max_span * 0.15,
          text      = "<b>Outflows</b>",
          showarrow = FALSE,
          xref      = "x", yref = "y",
          font      = list(color = "#2d224e", size = 14, family = "Arial Black"),
          xanchor   = "left"
        ),
        list(
          x         = 0,
          y         = -max_span * 0.15,
          text      = "<b>Inflows</b>",
          showarrow = FALSE,
          xref      = "x", yref = "y",
          font      = list(color = "#2d224e", size = 14, family = "Arial Black"),
          xanchor   = "left"
        )
      )
    ) |>
    plotly::config(displayModeBar = FALSE)
}
