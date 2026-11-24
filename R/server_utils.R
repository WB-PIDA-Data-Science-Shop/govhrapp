#' Update the subgroup filter choices
#'
#' Refills the subgroup picker whenever the selected grouping column changes.
#' The distinct values of each column are read once and reused afterward.
#'
#' @param .data Data frame holding the columns available for grouping.
#' @param input Shiny input object carrying `group_filter`.
#' @param session Shiny session object.
#'
#' @returns Invisibly, the observer created for the picker. Called for its side
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

#' Filter data by group, subgroup and date range
#'
#' Applies the sidebar selections to the panel data. Each selection is optional:
#' a `group_filter` of `"ref_date"` or `NULL` skips the subgroup step, and a
#' `NULL` `date_range` skips the date step.
#'
#' @param .data Data frame or database table (`tbl_dbi`) to filter.
#' @param group_filter Character. Grouping column to filter on, or `"ref_date"`
#'   or `NULL` to skip subgroup filtering.
#' @param subgroup_filter Character vector of subgroup values to keep.
#' @param date_range Length-two vector of dates bounding `ref_date`, or `NULL`
#'   to skip date filtering.
#'
#' @returns A filtered data frame of the same class as `.data`.
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
    # injected as values, since dbplyr would translate `date_range[1]` into
    # SQL over the whole vector
    filtered <- filtered |>
      dplyr::filter(
        .data[["ref_date"]] >= !!date_range[1],
        .data[["ref_date"]] <= !!date_range[2]
      )
  }

  filtered
}

#' Convert the no-grouping selection to `NULL`
#'
#' The sidebar group picker uses `"ref_date"` to mean "no grouping". The govhr
#' plotting functions accept that convention, but the compute functions that
#' take `group_cols` do not: [govhr::compute_movement()],
#' [govhr::compute_headcount()] and [govhr::compute_wagebill()] reject
#' `"ref_date"`, and [govhr::compute_percentile()] computes shares within each
#' date, which stack when plotted. This translates the picker value before it
#' reaches them.
#'
#' @param group_col Character or `NULL`. The selected grouping column, such as
#'   `input$group_filter`.
#'
#' @returns `NULL` when `group_col` is `NULL` or `"ref_date"`, otherwise
#'   `group_col` unchanged.
#'
#' @examples
#' \dontrun{
#' group_col_to_null("ref_date") # NULL
#' group_col_to_null("gender") # "gender"
#' }
#'
#' @keywords internal
group_col_to_null <- function(group_col) {
  if (is.null(group_col) || identical(group_col, "ref_date")) {
    NULL
  } else {
    group_col
  }
}

#' Summarize a movement value box
#'
#' Reads the latest count and rate for one movement type from precomputed
#' movement and retirement tables, for the workforce key indicator boxes. The
#' replacement box reports a ratio only.
#'
#' @param movement Output of [govhr::compute_movement()].
#' @param retirement Output of [govhr::compute_retirement()].
#' @param movement_type Character. One of `"hire"`, `"separation"`,
#'   `"retirement"` or `"replacement"`.
#'
#' @returns A list with `ref_date` (the date the values are from), `count` and
#'   `rate`.
#'
#' @importFrom dplyr collect filter slice_max
#' @keywords internal
summarize_movement_box <- function(movement, retirement, movement_type) {
  rate_col <- movement_measure_col(movement_type, "rate")

  # each box reports the latest date its own measure is defined for. a date
  # with no hires and no separations has a 0/0 = NaN replacement rate, which
  # R's is.na() drops but SQL's IS NOT NULL keeps, so the table is collected
  # before filtering. it holds one row per date, so collecting first is cheap
  latest <- (if (movement_type == "retirement") retirement else movement) |>
    dplyr::collect() |>
    dplyr::filter(!is.na(.data[[rate_col]])) |>
    dplyr::slice_max(.data[["ref_date"]], n = 1)

  list(
    ref_date = latest[["ref_date"]],
    count = if (movement_type == "replacement") {
      NA_real_
    } else {
      latest[[movement_measure_col(movement_type, "count")]]
    },
    rate = latest[[rate_col]]
  )
}

#' Summarize a wage bill value box
#'
#' Totals gross salary at the latest reference date for one employment status,
#' for the wage bill key indicator boxes.
#'
#' @param .data Contract data, as `wagebill_data` in
#'   [run_govhrapp_analytics()].
#' @param measure_type Character. `"total_wagebill"` (active personnel) or
#'   `"total_pension_liabilities"` (pensioners).
#'
#' @returns A list with `ref_date` and `total`.
#'
#' @importFrom dplyr filter pull summarise
#' @importFrom govhr compute_wagebill
#' @keywords internal
summarize_wagebill_box <- function(.data, measure_type) {
  status <- switch(
    measure_type,
    total_wagebill = "active",
    total_pension_liabilities = "pensioner",
    stop(
      "Invalid measure_type. Must be 'total_wagebill' or ",
      "'total_pension_liabilities'."
    )
  )

  latest_date <- .data |>
    dplyr::summarise(ref_date = max(.data[["ref_date"]], na.rm = TRUE)) |>
    dplyr::pull(.data[["ref_date"]])

  total <- .data |>
    dplyr::filter(
      .data[["ref_date"]] == !!latest_date,
      .data[["employment_status"]] == status
    ) |>
    govhr::compute_wagebill(measure_col = "gross_salary_lcu") |>
    dplyr::pull(.data[["wagebill"]])

  list(
    ref_date = latest_date,
    total = total
  )
}

#' Build the workforce analytics cache
#'
#' Precomputes the summaries the workforce panels show before the user applies
#' a filter.
#'
#' @inheritParams workforce_ui
#'
#' @returns A named list of precomputed data frames, keyed by panel.
#'
#' @importFrom govhr compute_headcount compute_movement compute_retirement
#'   compute_transition project_retirement
#' @importFrom purrr map set_names
#' @keywords internal
build_workforce_cache <- function(workforce_data, wagebill_data) {
  cache <- list(
    # overview module
    workforce_overview = govhr::compute_headcount(workforce_data),

    # transition module
    workforce_transition = wagebill_data |>
      govhr::compute_transition(
        id_col = "personnel_id",
        group_col = "contract_type"
      ),

    # retirement module
    workforce_retirement = govhr::compute_retirement(workforce_data),
    workforce_retirement_expected = workforce_data |>
      govhr::project_retirement(threshold_age = 60),

    # movement module
    workforce_movement = govhr::compute_movement(workforce_data),

    # movement profile
    workforce_movement_profile = workforce_data |>
      render_movement_profile(movement_type = "hire")
  ) |>
    collect_cache()

  # key indicator boxes, read from the collected movement and retirement
  # tables so their queries are not run again for each box
  cache[["movement_box"]] <- c(
    "hire",
    "separation",
    "retirement",
    "replacement"
  ) |>
    purrr::set_names() |>
    purrr::map(
      \(type) {
        summarize_movement_box(
          cache[["workforce_movement"]],
          cache[["workforce_retirement"]],
          type
        )
      }
    )

  cache
}

#' Build the wage bill analytics cache
#'
#' Precomputes the summaries the wage bill panels show before the user applies
#' a filter.
#'
#' @inheritParams wagebill_ui
#'
#' @returns A named list of precomputed data frames, keyed by panel.
#'
#' @importFrom dplyr left_join
#' @importFrom govhr compute_compression_ratio compute_decile compute_movement_cost
#'   compute_percentile compute_retirement_cost compute_wage compute_wagebill
#'   project_retirement
#' @importFrom purrr map set_names
#' @keywords internal
build_wagebill_cache <- function(wagebill_data) {
  list(
    # key indicator boxes
    total_box = c("total_wagebill", "total_pension_liabilities") |>
      purrr::set_names() |>
      purrr::map(\(type) summarize_wagebill_box(wagebill_data, type)),

    # overview module
    wagebill_overview = wagebill_data |>
      govhr::compute_wagebill(measure_col = "gross_salary_lcu") |>
      dplyr::left_join(
        govhr::compute_wage(wagebill_data, measure_col = "gross_salary_lcu"),
        by = "ref_date"
      ),

    # retirement module
    wagebill_retirement = wagebill_data |>
      govhr::compute_retirement_cost(measure_col = "gross_salary_lcu"),
    wagebill_retirement_expected = wagebill_data |>
      govhr::project_retirement(measure_col = "gross_salary_lcu"),

    # equity module
    wagebill_equity_percentile = wagebill_data |>
      govhr::compute_percentile(
        measure_col = "gross_salary_lcu",
        latest_measure = FALSE
      ),
    wagebill_equity_decile = wagebill_data |>
      govhr::compute_decile(
        measure_col = "gross_salary_lcu",
        latest_measure = TRUE
      ),
    wagebill_equity_compression = wagebill_data |>
      govhr::compute_compression_ratio(measure_col = "gross_salary_lcu"),

    # movement module
    wagebill_movement = wagebill_data |>
      govhr::compute_movement_cost(
        event_type = "hire",
        measure_col = "gross_salary_lcu"
      )
  ) |>
    collect_cache()
}

#' Collect the lazy entries of a cache
#'
#' Brings every lazy (`tbl_lazy`) entry of a cache list into memory, and leaves
#' data frames, value box lists and rendered tables as they are.
#'
#' @param cache Named list of precomputed summaries.
#'
#' @returns `cache`, with its lazy tables collected.
#'
#' @importFrom dplyr collect
#' @importFrom purrr modify_if
#' @keywords internal
collect_cache <- function(cache) {
  # the cache is built once and shared by every session, so a database query
  # runs here once instead of on every session's first render
  purrr::modify_if(
    cache,
    \(entry) inherits(entry, "tbl_lazy"),
    dplyr::collect
  )
}

#' Check that there are transitions to plot
#'
#' Shows a message in place of the transition plots when the selection leaves
#' no transitions. A narrow subgroup or date selection, or deselecting every
#' subgroup, can leave none, which [govhr::plot_trend()] errors on and the
#' network draws blank.
#'
#' @param transition_data Data frame of transitions, as returned by
#'   [govhr::compute_transition()].
#'
#' @importFrom shiny validate need
#' 
#' @returns Invisibly, the result of [shiny::validate()].
#' 
#' @keywords internal
validate_transitions <- function(transition_data) {
  shiny::validate(
    shiny::need(
      nrow(transition_data) > 0,
      "No transitions for the selected groups and dates."
    )
  )
}

#' Build the analytics cache
#'
#' Precomputes the summaries each panel of the analytics app shows before the
#' user applies a filter, so the app starts without aggregating anything. Build
#' it once and pass it to [run_govhrapp_analytics()], for example when several
#' apps or reports share the same data.
#'
#' @inheritParams run_govhrapp_analytics
#'
#' @returns A named list with `workforce` and `wagebill` elements, each a list of
#'   precomputed data frames keyed by panel.
#'
#' @examples
#' \dontrun{
#' cache <- build_analytics_cache(workforce_data, wagebill_data)
#' run_govhrapp_analytics(workforce_data, wagebill_data, cache = cache)
#' }
#'
#' @export
build_analytics_cache <- function(workforce_data, wagebill_data) {
  check_analytics_data(workforce_data, wagebill_data)

  list(
    workforce = build_workforce_cache(workforce_data, wagebill_data),
    wagebill = build_wagebill_cache(wagebill_data)
  )
}
