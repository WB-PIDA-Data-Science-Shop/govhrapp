#' Read the distinct values of a column
#'
#' Lists the non-missing values of one column, sorted, for the subgroup picker.
#'
#' @param .data Data frame or database table (`tbl_dbi`).
#' @param column Character. Name of the column to read.
#'
#' @returns A sorted character vector of the distinct non-missing values.
#'
#' @importFrom collapse funique
#' @importFrom dplyr all_of collect distinct select
#' @keywords internal
read_distinct_values <- function(.data, column) {
  values <- if (inherits(.data, "tbl_lazy")) {
    # `[[` cannot read a lazy table's column, and the database returns only
    # the distinct values rather than every row
    distinct_rows <- .data |>
      dplyr::select(dplyr::all_of(column)) |>
      dplyr::distinct() |>
      dplyr::collect()

    distinct_rows[[column]]
  } else {
    collapse::funique(.data[[column]])
  }

  sort(as.character(values[!is.na(values)]))
}

# the sidebar inputs that each pick a column and the values of it to keep, in
# sidebar order: each picker offers only the values left by the filters above
# it. shared by the picker updates and the filters, so a further filter needs
# only one more pair here and its inputs in the sidebar
filter_inputs <- list(
  c(column = "filter_col", values = "filter_values"),
  c(column = "group_filter", values = "subgroup_filter")
)

#' Check whether a column selection is set
#'
#' @param column Character or `NULL`. A column selection, such as
#'   `input$group_filter`.
#'
#' @returns `FALSE` for `NULL`, `"none"` and `"ref_date"`, the values meaning
#'   no selection, and `TRUE` otherwise.
#'
#' @keywords internal
is_column_selected <- function(column) {
  !is.null(column) && !column %in% c("none", "ref_date")
}

#' Read the values a sidebar picker offers
#'
#' Lists the values of the picker's column among the records left by the date
#' range and the filters above it in the sidebar, so the subgroups offered are
#' those of the filtered population.
#'
#' @param .data Data frame or database table (`tbl_dbi`) holding the columns
#'   available for filtering.
#' @param input Shiny input object, or a list, carrying `date_range` and the
#'   inputs in `filter_inputs`.
#' @param position Integer. Position of the picker's pair in `filter_inputs`.
#' @param cache Environment of values already read, keyed by the column and the
#'   selections above it.
#'
#' @returns A sorted character vector of values, empty when no column is
#'   selected.
#'
#' @keywords internal
read_picker_choices <- function(.data, input, position, cache) {
  column <- input[[filter_inputs[[position]][["column"]]]]

  if (!is_column_selected(column)) {
    return(character(0))
  }

  upstream <- selected_filters(input, filter_inputs[seq_len(position - 1)])

  # returning to an earlier selection, such as a previous date range, reuses
  # its values rather than reading them again, which on a database table
  # saves a query
  key <- paste(deparse(list(column, upstream, input$date_range)), collapse = "")

  if (is.null(cache[[key]])) {
    cache[[key]] <- .data |>
      filter_data(filters = upstream, date_range = input$date_range) |>
      read_distinct_values(column)
  }

  cache[[key]]
}

#' Update the value pickers of the sidebar filters
#'
#' Refills the population filter and subgroup pickers whenever their column or
#' a selection above them changes, with every value selected.
#'
#' @param .data Data frame or database table (`tbl_dbi`) holding the columns
#'   available for filtering.
#' @param input Shiny input object carrying `date_range` and the inputs in
#'   `filter_inputs`.
#' @param session Shiny session object.
#'
#' @returns Called for its side effect on the UI.
#'
#' @importFrom purrr iwalk
#' @importFrom shiny observe
#' @importFrom shinyWidgets updatePickerInput
#' @keywords internal
update_filter_controls <- function(.data, input, session) {
  cache <- new.env(parent = emptyenv())

  purrr::iwalk(filter_inputs, \(pair, position) {
    shiny::observe({
      values <- read_picker_choices(.data, input, position, cache)

      shinyWidgets::updatePickerInput(
        session,
        pair[["values"]],
        choices = values,
        selected = values
      )
    })
  })
}

#' Read the sidebar filter selections
#'
#' @param input Shiny input object, or a list, carrying the inputs in `pairs`.
#' @param pairs List of column and values input IDs to read. Default all of
#'   `filter_inputs`.
#'
#' @returns A list with one entry per set filter, each a list of `column` and
#'   the `values` of it to keep, as taken by [filter_data()].
#'
#' @importFrom purrr keep map
#' @keywords internal
selected_filters <- function(input, pairs = filter_inputs) {
  pairs |>
    purrr::keep(\(pair) is_column_selected(input[[pair[["column"]]]])) |>
    purrr::map(\(pair) {
      list(
        column = input[[pair[["column"]]]],
        values = input[[pair[["values"]]]]
      )
    })
}

#' Filter data by column values and date range
#'
#' Applies the sidebar selections to the panel data. Filters on the same column
#' combine, keeping only the values both allow.
#'
#' @param .data Data frame or database table (`tbl_dbi`) to filter.
#' @param filters List of filters, each a list of `column` and the `values` of
#'   it to keep, as returned by [selected_filters()]. Default none.
#' @param date_range Length-two vector of dates bounding `ref_date`, or `NULL`
#'   (default) to skip date filtering.
#'
#' @returns A filtered data frame of the same class as `.data`.
#'
#' @importFrom dplyr filter
#' @importFrom purrr reduce
#' @keywords internal
filter_data <- function(.data, filters = list(), date_range = NULL) {
  filtered <- purrr::reduce(
    filters,
    \(rows, selection) {
      column <- selection[["column"]]
      values <- selection[["values"]]

      dplyr::filter(rows, .data[[column]] %in% values)
    },
    .init = .data
  )

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

#' Check whether a table has any records
#'
#' @param .data Data frame or database table (`tbl_dbi`).
#'
#' @returns `TRUE` when `.data` has at least one row.
#'
#' @importFrom dplyr collect
#' @keywords internal
has_records <- function(.data) {
  if (inherits(.data, "tbl_lazy")) {
    # a one-row query rather than a count, so the database stops at the first
    # matching record instead of counting them all
    .data |>
      utils::head(1) |>
      dplyr::collect() |>
      nrow() > 0
  } else {
    nrow(.data) > 0
  }
}

#' Apply the sidebar filters to the panel data
#'
#' Filters `.data` by the sidebar's column filters and date range, and stops
#' with a message when no record is left.
#'
#' @param .data Data frame or database table (`tbl_dbi`) to filter.
#' @param input Shiny input object carrying the inputs in `filter_inputs` and
#'   `date_range`.
#'
#' @returns `.data` filtered, as returned by [filter_data()].
#'
#' @importFrom shiny need validate
#' @keywords internal
apply_sidebar_filters <- function(.data, input) {
  filtered <- filter_data(
    .data,
    filters = selected_filters(input),
    date_range = input$date_range
  )

  # an empty selection would otherwise reach the compute and plot functions,
  # which draw blank charts or warn rather than say why. raised here, the
  # message replaces every output that reads the filtered data
  shiny::validate(
    shiny::need(
      has_records(filtered),
      "No records match the selected filters and dates."
    )
  )

  filtered
}

#' Keep only active personnel
#'
#' The headcount, wage bill and pay distribution describe the current
#' workforce, so pensioners are dropped before people are counted or their pay
#' is summarized, as in the wage bill key indicator box.
#'
#' @param .data Data frame or database table (`tbl_dbi`) with an
#'   `employment_status` column.
#'
#' @returns `.data` restricted to rows whose `employment_status` is
#'   `"active"`, of the same class as `.data`.
#'
#' @importFrom dplyr filter
#' @keywords internal
filter_active <- function(.data) {
  dplyr::filter(.data, .data[["employment_status"]] == "active")
}

#' Keep only pensioners
#'
#' Retirees stay in the data as records with an `employment_status` of
#' `"pensioner"`, so their headcount and pension payments are read from those
#' records alone.
#'
#' @inheritParams filter_active
#'
#' @returns `.data` restricted to rows whose `employment_status` is
#'   `"pensioner"`, of the same class as `.data`.
#'
#' @importFrom dplyr filter
#' @keywords internal
filter_pensioners <- function(.data) {
  dplyr::filter(.data, .data[["employment_status"]] == "pensioner")
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
    workforce_overview = workforce_data |>
      filter_active() |>
      govhr::compute_headcount(),

    # transition module
    workforce_transition = wagebill_data |>
      govhr::compute_transition(
        id_col = "personnel_id",
        group_col = "contract_type"
      ),

    # retirement module
    workforce_pensioners = workforce_data |>
      filter_pensioners() |>
      govhr::compute_headcount(),
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
  active_contracts <- filter_active(wagebill_data)

  list(
    # key indicator boxes
    total_box = c("total_wagebill", "total_pension_liabilities") |>
      purrr::set_names() |>
      purrr::map(\(type) summarize_wagebill_box(wagebill_data, type)),

    # overview module
    wagebill_overview = active_contracts |>
      govhr::compute_wagebill(measure_col = "gross_salary_lcu") |>
      dplyr::left_join(
        govhr::compute_wage(active_contracts, measure_col = "gross_salary_lcu"),
        by = "ref_date"
      ),

    # retirement module
    wagebill_pensions = wagebill_data |>
      filter_pensioners() |>
      govhr::compute_wagebill(measure_col = "gross_salary_lcu"),
    wagebill_retirement = wagebill_data |>
      govhr::compute_retirement_cost(measure_col = "gross_salary_lcu"),
    wagebill_retirement_expected = wagebill_data |>
      govhr::project_retirement(measure_col = "gross_salary_lcu"),

    # equity module
    wagebill_equity_percentile = active_contracts |>
      govhr::compute_percentile(
        measure_col = "gross_salary_lcu",
        latest_measure = FALSE
      ),
    wagebill_equity_decile = active_contracts |>
      govhr::compute_decile(
        measure_col = "gross_salary_lcu",
        latest_measure = TRUE
      ),
    wagebill_equity_compression = active_contracts |>
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

#' Build a Shiny app, optionally behind a login
#'
#' Wraps the app in a shinymanager login screen when `secure = TRUE`, so the
#' analytics and quality check apps protect themselves the same way.
#'
#' @param ui A Shiny UI definition.
#' @param server A Shiny server function.
#' @param secure Logical. Require users to log in.
#' @param credentials_path Character. Path to the SQLite database of user
#'   credentials, read when `secure = TRUE`.
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @returns A Shiny app object.
#'
#' @importFrom shiny shinyApp
#' @importFrom shinymanager check_credentials secure_app secure_server
#' @keywords internal
shiny_app_with_login <- function(ui, server, secure, credentials_path, ...) {
  if (!secure) {
    return(shiny::shinyApp(ui, server, ...))
  }

  # checked before the app starts, since shinymanager only reads the database
  # once someone tries to log in, and would fail there instead
  if (!file.exists(credentials_path)) {
    stop(
      "No credentials database found at '", credentials_path, "'. ",
      "Create one with shinymanager::create_db(), or pass the path of an ",
      "existing one as `credentials_path`.",
      call. = FALSE
    )
  }

  secured_server <- function(input, output, session) {
    shinymanager::secure_server(
      check_credentials = shinymanager::check_credentials(credentials_path)
    )

    server(input, output, session)
  }

  shiny::shinyApp(
    shinymanager::secure_app(ui, enable_admin = TRUE),
    secured_server,
    ...
  )
}
