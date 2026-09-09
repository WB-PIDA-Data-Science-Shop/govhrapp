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

# ---- Plots -------------------------------------------------------------------

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
