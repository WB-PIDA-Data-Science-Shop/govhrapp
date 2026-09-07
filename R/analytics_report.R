#' Generate the Standard Analytics Report
#'
#' Renders a Word document from the same pre-computed summaries the analytics
#' app serves. Every figure is drawn from [build_analytics_cache()] at its
#' default (`ref_date`) grouping, so the report reproduces exactly what each
#' panel shows before the user applies a filter.
#'
#' @param workforce_data Data frame with workforce/personnel attributes
#'   (headcount).
#' @param wagebill_data Data frame with contract/salary attributes (wage bill).
#'
#' @return Character path to the generated Word document.
#'
#' @examples
#' \dontrun{
#' generate_analytics_report(workforce_data, wagebill_data)
#' }
#'
#' @importFrom govhr fastcount
#' @importFrom purrr pluck
#' @importFrom rmarkdown render
#' @importFrom here here
#' @export
generate_analytics_report <- function(workforce_data, wagebill_data) {
  cache <- build_analytics_cache(workforce_data, wagebill_data)

  # the value-box summarisers pull a column, so an empty result is length 0
  as_scalar <- function(x) if (length(x) == 1) as.numeric(x) else NA_real_

  movement_box <- purrr::pluck(cache, "workforce", "movement_box")
  workforce_indicators <- data.frame(
    Movement = c("Hires", "Separations", "Retirements", "Turnover"),
    Count = vapply(movement_box, \(box) as_scalar(box$count), numeric(1)),
    Rate = vapply(movement_box, \(box) as_scalar(box$rate), numeric(1)),
    row.names = NULL
  )

  total_box <- purrr::pluck(cache, "wagebill", "total_box")
  wagebill_indicators <- data.frame(
    Indicator = c("Total wage bill", "Total pension liabilities"),
    Total = vapply(total_box, \(box) as_scalar(box$total), numeric(1)),
    row.names = NULL
  )

  # each plot mirrors the call its panel makes against the same cache entry
  plots <- list(
    workforce_overview = purrr::pluck(cache, "workforce", "workforce_overview") |>
      plot_trend(group_col = "ref_date", y_label = "Headcount"),

    workforce_movement = purrr::pluck(cache, "workforce", "workforce_movement") |>
      plot_movement(
        movement_type = "hire",
        measurement_type = "count",
        group_col = "ref_date"
      ),

    workforce_transition = purrr::pluck(cache, "workforce", "workforce_transition") |>
      govhr::fastcount(ref_date, name = "transition") |>
      plot_trend(
        group_col = "ref_date",
        y_col = "transition",
        y_label = "Number of Transitions"
      ),

    workforce_retirement = purrr::pluck(cache, "workforce", "workforce_retirement") |>
      plot_movement(
        movement_type = "retirement",
        measurement_type = "count",
        group_col = "ref_date"
      ),

    workforce_retirement_expected = purrr::pluck(
      cache,
      "workforce",
      "workforce_retirement_expected"
    ) |>
      plot_movement(
        movement_type = "retirement",
        measurement_type = "count",
        group_col = "ref_date"
      ),

    wagebill_overview = purrr::pluck(cache, "wagebill", "wagebill_overview") |>
      plot_trend(group_col = "ref_date", y_label = "Wage Bill"),

    wagebill_density = purrr::pluck(cache, "wagebill", "wagebill_equity_percentile") |>
      plot_histogram(plot_type = "histogram", group_col = "ref_date"),

    wagebill_decile = purrr::pluck(cache, "wagebill", "wagebill_equity_decile") |>
      plot_decile(group_col = "ref_date"),

    wagebill_compression = purrr::pluck(cache, "wagebill", "wagebill_equity_compression") |>
      plot_compression_ratio(group_col = "ref_date"),

    wagebill_movement = purrr::pluck(cache, "wagebill", "wagebill_movement") |>
      plot_trend(
        group_col = "ref_date",
        y_col = "movement_cost",
        y_label = "Movement Costs"
      ),

    wagebill_retirement = purrr::pluck(cache, "wagebill", "wagebill_retirement") |>
      plot_trend(
        group_col = "ref_date",
        y_col = "movement_cost",
        y_label = "Retirement Costs"
      ),

    wagebill_retirement_expected = purrr::pluck(
      cache,
      "wagebill",
      "wagebill_retirement_expected"
    ) |>
      plot_trend(
        group_col = "ref_date",
        y_col = "projected_cost",
        y_label = "Projected Retirement Costs"
      )
  )

  temp_report <- file.path(tempdir(), "analytics_report.qmd")
  file.copy(
    system.file("markdown/analytics_report.qmd", package = "govhrapp"),
    temp_report,
    overwrite = TRUE
  )

  output_file <- file.path(
    getwd(),
    paste0("analytics_report_", format(Sys.Date(), "%Y%m%d"), ".docx")
  )

  rmarkdown::render(
    input = temp_report,
    output_file = output_file,
    params = list(
      ref_dates = range(workforce_data[["ref_date"]], na.rm = TRUE),
      workforce_indicators = workforce_indicators,
      wagebill_indicators = wagebill_indicators,
      plots = plots
    ),
    envir = new.env(),
    quiet = TRUE
  )

  output_file
}
