#' Identify the available pay measures
#'
#' Lists the salary and allowance columns present in the data, nested by
#' dictionary module, for the pay measure picker.
#'
#' @param .data Contract data, as `wagebill_data` in
#'   [run_govhrapp_analytics()].
#'
#' @returns A named list of pay measure choices, keyed by module.
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @keywords internal
identify_wagebill_choices <- function(.data) {
  # colnames() rather than names(), which does not list a tbl_dbi's columns
  available_cols <- colnames(.data)

  govhr::dictionary |>
    dplyr::filter(
      .data[["variable_id"]] %in% available_cols,
      stringr::str_detect(.data[["variable_id"]], "salary|allowance")
    ) |>
    nest_choices_by_module()
}

#' Render a wage bill value box
#'
#' Renders the total wage bill or total pension liabilities at the latest
#' reference date. Values are read from the analytics cache when available and
#' computed from `.data` otherwise.
#'
#' @param .data Contract data, as `wagebill_data` in
#'   [run_govhrapp_analytics()].
#' @param measure_type Character. `"total_wagebill"` or
#'   `"total_pension_liabilities"`.
#' @param cache List of precomputed summaries from [build_analytics_cache()],
#'   or `NULL` (default) to compute the values from `.data`.
#'
#' @returns A Shiny render function producing the value box.
#'
#' @importFrom bsicons bs_icon
#' @importFrom bslib value_box value_box_theme
#' @importFrom purrr pluck
#' @importFrom scales comma
#' @importFrom shiny renderUI
#' @keywords internal
render_wagebill_box <- function(.data, measure_type, cache = NULL) {
  values <- purrr::pluck(cache, "wagebill", "total_box", measure_type)

  if (is.null(values)) {
    values <- summarize_wagebill_box(.data, measure_type)
  }

  label <- switch(
    measure_type,
    total_wagebill = "Total Wage Bill",
    total_pension_liabilities = "Total Pension Liabilities"
  )

  shiny::renderUI({
    bslib::value_box(
      title = paste0(label, " (", values[["ref_date"]], ")"),
      value = scales::comma(values[["total"]], accuracy = 1),
      showcase = bsicons::bs_icon(
        switch(
          measure_type,
          total_wagebill = "currency-dollar",
          total_pension_liabilities = "piggy-bank-fill"
        )
      ),
      theme = bslib::value_box_theme(bg = "#C34729", fg = "#ffffff"),
      class = "border",
      max_height = "150px"
    )
  })
}
