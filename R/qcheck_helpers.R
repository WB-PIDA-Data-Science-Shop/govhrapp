#' Render a Data Coverage Value Box
#'
#' @param .data Data frame. A dataframe to compute the coverage on.
#' @param title String. Value box title.
#' @param icon String. Icon name. Defaults to `"table"`.
#'
#' @return A [shiny::renderUI()] producing a [bslib::value_box()] themed
#'   `"danger"` (<50%), `"warning"` (50–79%), or `"success"` (≥80%).
#'
#' @importFrom shiny renderUI
#' @importFrom bslib value_box
#' @importFrom dplyr case_when
#' @importFrom bsicons bs_icon
#' @importFrom govhr compute_global_coverage
#'
#' @details Coverage denotes the total number of non-missing records in a dataset, expressed as a percentage of the total records. For example, if a dataset has 10 rows and 10 columns, i.e., 100 records, and 10 of them are available, the coverage would be 10 percent.
#' @export
render_coverage_box <- function(.data, title, icon = "table") {
  shiny::renderUI({
    value_coverage <- govhr::compute_global_coverage(.data)

    theme <- dplyr::case_when(
      value_coverage < 50 ~ "danger",
      value_coverage >= 50 & value_coverage < 80 ~ "warning",
      TRUE ~ "success"
    )

    bslib::value_box(
      title = title,
      value = paste(value_coverage, "%"),
      showcase = bsicons::bs_icon(icon),
      theme = theme
    )
  })
}

#' Render a Data Consistency Value Box
#'
#' @param .data Data frame. A dataframe to compute the consistency on.
#' @param title String. Value box title.
#' @param id_col String. The name of the column that uniquely identifies records (e.g., "personnel_id" or "contract_id").
#' @param value_cols Character vector. The names of the columns whose values are to be checked for consistency.
#' @param icon String. Icon name. Defaults to `"table"`.
#'
#' @return A [shiny::renderUI()] producing a [bslib::value_box()] themed
#'   `"danger"` (<50%), `"warning"` (50–79%), or `"success"` (≥80%).
#'
#' @importFrom shiny renderUI p
#' @importFrom bslib value_box
#' @importFrom bsicons bs_icon
#' @importFrom dplyr case_when
#' @importFrom purrr map_dbl
#' @importFrom dplyr pull
#'
#' @details Consistency denotes the proportion of records and values that are consistent across the dataset. A record is considered consistent if it has a unique identifier and all its associated values are consistent. A value is considered consistent if it does not contradict other values for the same record.
#' @export
render_consistency_box <- function(
  .data,
  id_col,
  value_cols,
  title,
  icon = "table"
) {
  shiny::renderUI({
    consistency <- govhr::compute_global_consistency(.data, id_col, value_cols)

    consistency_record <- govhr::compute_record_consistency(.data, id_col) |>
      dplyr::pull(.data[["record_consistency"]])

    consistency_value <- purrr::map_dbl(
      value_cols,
      \(value_col) {
        govhr::compute_value_consistency(.data, id_col, value_col) |>
          dplyr::pull(.data[["value_consistency"]])
      }
    ) |>
      mean(na.rm = TRUE)

    theme <- dplyr::case_when(
      consistency < 50 ~ "danger",
      consistency >= 50 & consistency < 80 ~ "warning",
      TRUE ~ "success"
    )

    bslib::value_box(
      title = title,
      value = paste0(consistency, "%"),
      showcase = bsicons::bs_icon(icon),
      theme = theme,
      p(paste0("Record consistency: ", consistency_record, "%")),
      p(paste0("Value consistency: ", consistency_value, "%"))
    )
  })
}

#' Render a Data Validation Value Box
#'
#' @param validation_data Data frame. A dataframe containing the validation results, including the number of passes and total records.
#' @param title String. Value box title.
#' @param icon String. Icon name. Defaults to `"table"`.
#'
#' @return A [shiny::renderUI()] producing a [bslib::value_box()] themed
#'   `"danger"` (<50%), `"warning"` (50–79%), or `"success"` (≥80%).
#'
#' @importFrom shiny renderUI
#' @importFrom bslib value_box
#' @importFrom bsicons bs_icon
#' @importFrom dplyr case_when
#'
#' @export
render_validation_box <- function(validation_data, title, icon = "table") {
  shiny::renderUI({
    df <- validation_data
    total_passes <- sum(df$Passes, na.rm = TRUE)
    total_records <- sum(df$`Total Records`, na.rm = TRUE)

    pass_rate <- round(total_passes / total_records * 100, 2)

    theme <- dplyr::case_when(
      pass_rate < 50 ~ "danger",
      pass_rate >= 50 & pass_rate < 80 ~ "warning",
      TRUE ~ "success"
    )

    bslib::value_box(
      title = title,
      value = paste0(pass_rate, "%"),
      showcase = bsicons::bs_icon(icon),
      theme = theme
    )
  })
}

#' Generate UI controls to filter data.
#' 
#' @param .data Data frame. The data to be filtered.
#' @param id Character string. The module namespace ID.
#'
#' @import shiny
#' @importFrom shinyWidgets pickerInput pickerOptions
#'  
#' @return A list of Shiny UI elements for filtering the data.
ui_filter_controls <- function(.data, id) {
  group_choices <- identify_group_choices(.data)

  list(
    shiny::dateRangeInput(
      shiny::NS(id, "date_range"),
      "Select date range:",
      start = min(.data[["ref_date"]], na.rm = TRUE),
      end = max(.data[["ref_date"]], na.rm = TRUE),
      min = min(.data[["ref_date"]], na.rm = TRUE),
      max = max(.data[["ref_date"]], na.rm = TRUE)
    ),
    shiny::selectInput(
      shiny::NS(id, "group_filter"),
      "Select group:",
      choices = group_choices
    ),
    # only show the subgroup filter if a group is selected and it's not "ref_date"
    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] !== 'none' && input['%s'] !== 'ref_date'",
        shiny::NS(id, "group_filter"),
        shiny::NS(id, "group_filter")
      ),
      # subgroup filter: dynamically populated based on the selected group
      shinyWidgets::pickerInput(
        shiny::NS(id, "subgroup_filter"),
        "Select subgroups:",
        choices = NULL,
        multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          selectedTextFormat = "count > 3",
          countSelectedText = "{0} subgroups selected",
          noneSelectedText = "No subgroups selected",
          container = "body"
        )
      )
    )
  )
}
