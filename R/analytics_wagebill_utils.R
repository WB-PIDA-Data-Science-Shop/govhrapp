#' Function to identify available wagebill measure choices from the data and dictionary
#' @param .data A data frame containing wagebill data.
#' 
#' @importFrom dplyr filter summarise pull
#' @importFrom purrr set_names
#' 
#' @return A named list of wagebill measure choices, where each element corresponds to a module and contains a named vector of variable IDs and their corresponding variable names.
identify_wagebill_choices <- function(.data) {
  available_cols <- names(.data)

  wagebill_choices <- govhr::dictionary |>
    dplyr::filter(
      .data[["variable_id"]] %in%
        available_cols &
        stringr::str_detect(.data[["variable_id"]], "salary|allowance")
    ) |>
    dplyr::summarise(
      choices = list(
        purrr::set_names(.data[["variable_id"]], .data[["variable_name"]])
      ),
      .by = .data[["module"]]
    ) |>
    dplyr::pull(.data[["choices"]], name = .data[["module"]])

  wagebill_choices
}

#' Function to render a wagebill summary value box.
#'
#' @param wagebill_data A data frame containing wagebill data.
#' @param type_measure A character string specifying which measure to display. Must be one of "total_wagebill" or "total_pension_liabilities".
#'
#' @import bslib
#' @importFrom shiny renderUI
#' @importFrom bsicons bs_icon
#' @importFrom scales comma
#' @importFrom dplyr filter pull
#'
#' @return A Shiny UI output rendering a value box summarizing the wage bill or pension liabilities total.
render_wagebill_box <- function(wagebill_data, type_measure) {
  measure_col <- "gross_salary_lcu"

  if (type_measure == "total_wagebill") {
    label <- "Total Wage Bill"

    total_value <- govhr::compute_fastsummary(
      wagebill_data |>
        dplyr::filter(.data[["ref_date"]] == max(.data[["ref_date"]]) & .data[["employment_status"]] == "active"),
      cols = measure_col,
      groups = "ref_date",
      fns = "sum"
    ) |>
      dplyr::pull(.data[["value"]])
  } else if (type_measure == "total_pension_liabilities") {
    label <- "Total Pension Liabilities"

    total_value <- govhr::compute_fastsummary(
      wagebill_data |>
        dplyr::filter(.data[["ref_date"]] == max(.data[["ref_date"]]) & .data[["employment_status"]] == "pensioner"),
      cols = measure_col,
      groups = "ref_date",
      fns = "sum"
    ) |>
      dplyr::pull(.data[["value"]])
  } else {
    stop("Invalid type_measure. Must be 'total_wagebill' or 'total_pension_liabilities'.")
  }

  shiny::renderUI({
    value_box(
      title = paste0(label, "(", max(wagebill_data[["ref_date"]]), ")"),
      value = scales::comma(total_value, accuracy = 1),
      showcase = switch(
        type_measure,
        "total_wagebill" = bsicons::bs_icon("currency-dollar"),
        "total_pension_liabilities" = bsicons::bs_icon("piggy-bank-fill")
      ),
      theme = value_box_theme(bg = "#C34729", fg = "#ffffff"),
      class = "border",
      max_height = "150px"
    )
  })
}