#' Update Subgroup Filter Choices
#'
#' Repopulates the subgroup picker whenever the selected grouping column
#' changes. Distinct values are scanned once per column and reused thereafter.
#'
#' @param .data Data frame holding the columns available for grouping.
#' @param input Shiny input object carrying `group_filter`.
#' @param session Shiny session object.
#'
#' @return Invisibly, the observer created for the picker. Called for its side
#'   effect on the UI.
#'
#' @importFrom collapse funique
#' @importFrom shiny observe
#' @importFrom shinyWidgets updatePickerInput
#' @keywords internal
update_group_filter_controls <- function(.data, input, session) {
  # distinct values are stable for the session, so scan each column only once
  subgroup_cache <- new.env(parent = emptyenv())

  subgroup_choices <- function(variable) {
    if (is.null(subgroup_cache[[variable]])) {
      values <- .data[[variable]]
      subgroup_cache[[variable]] <- sort(
        as.character(collapse::funique(values[!is.na(values)]))
      )
    }

    subgroup_cache[[variable]]
  }

  shiny::observe({
    variable <- input$group_filter

    if (is.null(variable) || variable %in% c("none", "ref_date")) {
      shinyWidgets::updatePickerInput(
        session,
        "subgroup_filter",
        choices = NULL,
        selected = character(0)
      )
    } else {
      filter_vals <- subgroup_choices(variable)

      shinyWidgets::updatePickerInput(
        session,
        "subgroup_filter",
        choices = filter_vals,
        selected = filter_vals
      )
    }
  })
}

#' Filter Data by Group, Subgroup and Date Range
#'
#' Applies the sidebar selections to the panel data. Each selection is optional:
#' a `"ref_date"` or `NULL` `group_filter` skips the subgroup step, and a `NULL`
#' `date_range` skips the date step.
#'
#' @param .data Data frame to filter.
#' @param group_filter Character. Grouping column to filter on, or `"ref_date"`
#'   / `NULL` to skip subgroup filtering.
#' @param subgroup_filter Character vector of subgroup values to keep.
#' @param date_range Length-two vector of dates bounding `ref_date`, or `NULL`
#'   to skip date filtering.
#'
#' @return A filtered data frame of the same class as `.data`.
#'
#' @importFrom dplyr filter
#' @keywords internal
filter_data <- function(.data, group_filter, subgroup_filter, date_range) {
  filtered <- .data

  if (!is.null(group_filter) && group_filter != "ref_date") {
    filtered <- filtered |>
      dplyr::filter(.data[[group_filter]] %in% subgroup_filter)
  }

  if (!is.null(date_range)) {
    filtered <- filtered |>
      dplyr::filter(
        .data[["ref_date"]] >= date_range[1],
        .data[["ref_date"]] <= date_range[2]
      )
  }

  filtered
}

#' Summarise a Movement Value Box
#'
#' Computes the latest count and rate for one movement type, used by the
#' workforce key-indicator boxes. Turnover reports a rate only.
#'
#' @param .data Data frame containing personnel data.
#' @param movement_type Character. One of `"hire"`, `"fire"`, `"retirement"` or
#'   `"turnover"`.
#'
#' @return A list with `ref_date`, `count` and `rate`.
#'
#' @importFrom dplyr filter pull
#' @importFrom govhr compute_workforce_movement
#' @importFrom stats na.omit
#' @keywords internal
summarise_movement_box <- function(.data, movement_type) {
  latest_indicator <- function(measurement_type) {
    govhr::compute_workforce_movement(
      data = .data,
      movement_type = movement_type,
      measurement_type = measurement_type,
      group_cols = "ref_date"
    ) |>
      stats::na.omit() |>
      dplyr::filter(.data[["ref_date"]] == max(.data[["ref_date"]])) |>
      dplyr::pull(.data[["indicator"]])
  }

  list(
    ref_date = max(.data[["ref_date"]], na.rm = TRUE),
    # the turnover box shows a ratio only, so its count is never computed
    count = if (movement_type == "turnover") NA_real_ else latest_indicator("count"),
    rate = latest_indicator("rate")
  )
}

#' Summarise a Wage Bill Value Box
#'
#' Totals gross salary at the latest reference date for one employment status,
#' used by the wage bill key-indicator boxes.
#'
#' @param .data Data frame containing wage bill data.
#' @param measure_type Character. Either `"total_wagebill"` (active personnel)
#'   or `"total_pension_liabilities"` (pensioners).
#'
#' @return A list with `ref_date` and `total`.
#'
#' @importFrom dplyr filter pull
#' @importFrom govhr compute_fastsummary
#' @keywords internal
summarise_wagebill_box <- function(.data, measure_type) {
  status <- switch(
    measure_type,
    total_wagebill = "active",
    total_pension_liabilities = "pensioner",
    stop(
      "Invalid measure_type. Must be 'total_wagebill' or ",
      "'total_pension_liabilities'."
    )
  )

  total <- .data |>
    dplyr::filter(
      .data[["ref_date"]] == max(.data[["ref_date"]]),
      .data[["employment_status"]] == status
    ) |>
    govhr::compute_fastsummary(
      cols = "gross_salary_lcu",
      group_cols = "ref_date",
      fns = "sum"
    ) |>
    dplyr::pull(.data[["value"]])

  list(
    ref_date = max(.data[["ref_date"]], na.rm = TRUE),
    total = total
  )
}

#' Build the Workforce Analytics Cache
#'
#' Pre-computes the summaries the workforce panels show before the user applies
#' any filter.
#'
#' @param workforce_data Data frame with workforce/personnel attributes
#'   (headcount).
#' @param wagebill_data Data frame with contract/salary attributes, used for the
#'   career transition network.
#'
#' @return A named list of pre-computed data frames keyed by panel.
#'
#' @importFrom dplyr rename
#' @importFrom govhr compute_trend_summary compute_workforce_movement detect_career_transition project_retirement
#' @importFrom purrr map set_names
#' @keywords internal
build_workforce_cache <- function(workforce_data, wagebill_data) {
  list(
    # key indicator boxes
    movement_box = c("hire", "fire", "retirement", "turnover") |>
      purrr::set_names() |>
      purrr::map(\(type) summarise_movement_box(workforce_data, type)),

    # overview module
    workforce_overview = workforce_data |>
      govhr::compute_trend_summary(group_col = "ref_date"),

    # transition module
    workforce_transition = wagebill_data |>
      govhr::detect_career_transition(
        id_col = "personnel_id",
        group_cols = "contract_type"
      ),

    # retirement module
    workforce_retirement = workforce_data |>
      govhr::compute_workforce_movement(
        movement_type = "retirement",
        measurement_type = "count",
        group_cols = "ref_date"
      ),
    workforce_retirement_expected = govhr::project_retirement(
      data = workforce_data,
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
      ),

    # movement profile
    workforce_movement_profile = workforce_data |>
      render_movement_profile(movement_type = "hire")
  )
}

#' Build the Wage Bill Analytics Cache
#'
#' Pre-computes the summaries the wage bill panels show before the user applies
#' any filter.
#'
#' @param wagebill_data Data frame with contract/salary attributes (wage bill).
#'
#' @return A named list of pre-computed data frames keyed by panel.
#'
#' @importFrom dplyr rename
#' @importFrom govhr compute_compression_ratio compute_decile compute_movement_cost compute_percentile compute_trend_summary project_retirement
#' @importFrom purrr map set_names
#' @keywords internal
build_wagebill_cache <- function(wagebill_data) {
  list(
    # key indicator boxes
    total_box = c("total_wagebill", "total_pension_liabilities") |>
      purrr::set_names() |>
      purrr::map(\(type) summarise_wagebill_box(wagebill_data, type)),

    # overview module
    wagebill_overview = wagebill_data |>
      govhr::compute_trend_summary(
        group_col = "ref_date",
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
      govhr::project_retirement(
        group_cols = "ref_date",
        measure_col = "gross_salary_lcu"
      ) |>
      dplyr::rename(ref_date = "retirement_date"),

    # equity module
    wagebill_equity_percentile = wagebill_data |>
      govhr::compute_percentile(
        binwidth = 100,
        measure_col = "gross_salary_lcu",
        latest_measure = FALSE
      ),
    wagebill_equity_decile = wagebill_data |>
      govhr::compute_decile(
        group_cols = "ref_date",
        measure_col = "gross_salary_lcu",
        latest_measure = TRUE
      ),
    wagebill_equity_compression = wagebill_data |>
      govhr::compute_compression_ratio(
        group_cols = NULL,
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
}

#' Build the Analytics Cache
#'
#' Pre-computes the summaries each analytics panel shows before the user applies
#' any filter, so app start-up and first paint do no aggregation. Built once by
#' the app entry point and passed to every module server.
#'
#' @param workforce_data Data frame with workforce/personnel attributes
#'   (headcount).
#' @param wagebill_data Data frame with contract/salary attributes (wage bill).
#'
#' @return A named list with `workforce` and `wagebill` elements, each a list of
#'   pre-computed data frames keyed by panel.
#'
#' @export
build_analytics_cache <- function(workforce_data, wagebill_data) {
  list(
    workforce = build_workforce_cache(workforce_data, wagebill_data),
    wagebill = build_wagebill_cache(wagebill_data)
  )
}
