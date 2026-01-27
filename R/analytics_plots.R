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