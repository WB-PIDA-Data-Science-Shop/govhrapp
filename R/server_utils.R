#' Update filter controls based on selected group variable
#'
#' @param .data A data frame containing the data to be filtered.
#' @param input A Shiny input object containing the selected group variable.
#' @param session A Shiny session object.
#'
#' @importFrom shiny observe
#' @importFrom shinyWidgets updatePickerInput
#' @return None. This function updates the filter controls in the Shiny UI.
update_group_filter_controls <- function(.data, input, session) {
  shiny::observe({
    variable <- input$group_filter

    if (is.null(variable) || variable == "none") {
      shinyWidgets::updatePickerInput(
        session,
        "subgroup_filter",
        choices = NULL,
        selected = character(0)
      )
    } else {
      filter_vals <- sort(
        as.character(
          unique(
            stats::na.omit(.data[[variable]])
          )
        )
      )

      shinyWidgets::updatePickerInput(
        session,
        "subgroup_filter",
        choices = filter_vals,
        selected = filter_vals
      )
    }
  })
}

#' Filter data based on group, subgroup, and date range inputs
#'
#' @param data A data frame containing the data to be filtered.
#' @param group_filter A character string specifying the group variable to filter by.
#' @param subgroup_filter A character vector specifying the subgroups to filter by.
#' @param date_range A vector of two dates specifying the date range to filter by.
#'
#' @importFrom dplyr filter
#' @return A filtered data frame based on the specified inputs.
filter_data <- function(data, group_filter, subgroup_filter, date_range) {
  if (group_filter != "ref_date") {
    data <- data |>
      dplyr::filter(.data[[group_filter]] %in% subgroup_filter)
  }

  if (!is.null(date_range)) {
    data <- data |>
      dplyr::filter(
        .data[["ref_date"]] >= date_range[1],
        .data[["ref_date"]] <= date_range[2]
      )
  }

  data
}

#' Build cache for analytics app
#'
#' @param workforce_data A data frame containing workforce data.
#' @param wagebill_data A data frame containing wage bill data.
#'
#' @return A list containing cached data frames for improved performance.
#'
#' @importFrom dplyr filter select rename
#' @importFrom govhr compute_trend_summary compute_workforce_movement compute_movement_cost
build_analytics_cache <- function(workforce_data, wagebill_data) {
  # cache data to improve performance
  cache_workforce <- list(
    # workforce cache
    workforce_overview = workforce_data |>
      compute_trend_summary(
        group = "ref_date"
      ),

    # transfer module
    workforce_transfer = wagebill_data |>
      detect_career_transition(
        id_col = "personnel_id",
        group_cols = "paygrade"
      ),

    # retirement module
    workforce_retirement = workforce_data |>
      govhr::compute_workforce_movement(
        movement_type = "retirement",
        measurement_type = "count",
        group_cols = "ref_date"
      ),
    workforce_retirement_expected = project_retirement(
      .data = workforce_data,
      threshold_age = 60,
      birth_col = "birth_date",
      group_cols = "ref_date",
      simplify_retirement_date = TRUE
    ) |>
      dplyr::rename(ref_date = "retirement_date"),

    # movement module
    workforce_movement = workforce_data |>
      govhr::compute_workforce_movement(
        movement_type = "hire",
        measurement_type = "count",
        group_cols = "ref_date"
      )
  )

  cache_wagebill <- list(
    # wage bill cache
    wagebill_overview = wagebill_data |>
      compute_trend_summary(
        group = "ref_date",
        measure_col = "gross_salary_lcu"
      ),

    # retirement module
    wagebill_retirement = wagebill_data |>
      govhr::compute_movement_cost(
        event_type = "retirement",
        measure_col = "gross_salary_lcu",
        group_cols = "ref_date"
      ),
    wagebill_retirement_expected = wagebill_data |>
      project_retirement(
        group_cols = "ref_date",
        measure_col = "gross_salary_lcu"
      ) |>
      dplyr::rename(ref_date = "retirement_date"),

    # equity module
    wagebill_equity_percentile = wagebill_data |>
      compute_percentile(
        binwidth = 100,
        measure_col = "gross_salary_lcu",
        latest_measure = TRUE
      ),
    wagebill_equity_decile = wagebill_data |>
      compute_decile(
        group_cols = "ref_date",
        measure_col = "gross_salary_lcu",
        latest_measure = TRUE
      ),
    wagebill_equity_compression = wagebill_data |>
      compute_compression_ratio(
        group_col = NULL,
        measure_col = "gross_salary_lcu"
      ),

    # movement module
    wagebill_movement = wagebill_data |>
      govhr::compute_movement_cost(
        event_type = "hire",
        measure_col = "gross_salary_lcu",
        group_cols = "ref_date"
      )
  )

  cache_analytics <- list(
    workforce = cache_workforce,
    wagebill = cache_wagebill
  )

  cache_analytics
}
