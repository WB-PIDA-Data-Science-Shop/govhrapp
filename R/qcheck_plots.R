#' Plot Coverage Over Time
#'
#' Computes coverage using [compute_coverage()] (with `ref_date` always
#' included, aggregated across variables) and renders a trend line via
#' [plot_trend()].
#'
#' @param data A data frame. Typically the contract, personnel, or
#'   establishment dataset for the active module.
#' @param group Character string. Grouping variable inherited from the
#'   `coverage_group` UI input (e.g. `"ref_date"`, `"grade_id"`).
#' @param toggle_growth Logical. When `TRUE` the y-axis switches to a
#'   baseline-index view (first period = 100). Defaults to `FALSE`.
#'
#' @return A ggplot2 object.
#'
#' @keywords internal
plot_coverage_trend <- function(data, group, toggle_growth = FALSE) {
  coverage_data <- compute_coverage(
    data,
    group = group,
    include_ref_date = TRUE,
    aggregate = TRUE
  )

  plot_trend(
    coverage_data,
    y_col = "coverage",
    group = group,
    toggle_growth = toggle_growth,
    y_label = "Coverage"
  )
}

#' Plot Coverage by Group (Coloured Bar Chart)
#'
#' Computes per-group coverage using [compute_coverage()] (without
#' `ref_date`, not aggregated) and renders a horizontal bar chart coloured
#' green-yellow-red according to the same cutpoints used by the value boxes:
#'
#' * **High (>=80%)** — green (`#388e3c`)
#' * **Medium (50-79%)** — yellow (`#f9a825`)
#' * **Low (<50%)** — red (`#d32f2f`)
#'
#' @param data A data frame. Typically the contract, personnel, or
#'   establishment dataset for the active module.
#'
#' @return A ggplot2 object.
#'
#' @import ggplot2
#' @importFrom dplyr filter mutate case_when
#' @importFrom stats reorder
#' @importFrom stringr str_wrap
#' @importFrom scales label_percent
#'
#' @keywords internal
plot_coverage_bar <- function(data) {
  # compute coverage by variable and group, when chosen
  coverage_data <- compute_coverage(
    data,
    include_ref_date = FALSE,
    aggregate = FALSE
  ) |>
    dplyr::mutate(
      coverage_tier = dplyr::case_when(
        .data[["coverage"]] < 0.5 ~ "Low (<50%)",
        .data[["coverage"]] < 0.8 ~ "Medium (50-79%)",
        TRUE ~ "High (>=80%)"
      ),
      coverage_tier = factor(
        .data[["coverage_tier"]],
        levels = c("Low (<50%)", "Medium (50-79%)", "High (>=80%)")
      )
    )

  coverage_data |>
    dplyr::filter(
      !is.na(
        .data[["coverage"]]
      )
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
      labels = scales::label_percent(),
      limits = c(0, 1)
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Low (<50%)" = "#d32f2f",
        "Medium (50-79%)" = "#f9a825",
        "High (>=80%)" = "#388e3c"
      ),
      drop = FALSE
    ) +
    ggplot2::labs(x = "Coverage", y = "", fill = "Coverage")
}

#' Plot Coverage Heatmap by Group
#'
#' @param data A data frame.
#' @param group Character string. Grouping variable.
#'
#' @importFrom plotly plot_ly
#' @importFrom dplyr across everything group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom scales label_percent
#'
#' @return A ggplot2 object representing a heatmap of coverage values by group and variable.
plot_coverage_heatmap <- function(data, group = NULL) {
  if (is.null(group) || group == "none") {
    group <- "ref_date"
  }

  coverage_data <- compute_coverage(
    data,
    group = group,
    aggregate = FALSE
  )

  # plot heatmap
  plotly::plot_ly(
    data = coverage_data,
    x = ~ .data[[group]],
    y = ~variable,
    z = ~coverage,
    type = "heatmap",
    colorscale = list(c(0, "#d32f2f"), c(0.5, "#f9a825"), c(1, "#388e3c")),
    zmin = 0,
    zmax = 1,
    xgap = 2,
    ygap = 2,
    hovertemplate = paste0(
      "Group: %{x}<br>",
      "Variable: %{y}<br>",
      "Coverage: %{z:.0%}",
      "<extra></extra>"
    ),
    colorbar = list(
      title = "Coverage",
      tickformat = ".0%"
    )
  ) |>
    plotly::layout(
      xaxis = list(title = "Group"),
      yaxis = list(title = "Variable")
    )
}
