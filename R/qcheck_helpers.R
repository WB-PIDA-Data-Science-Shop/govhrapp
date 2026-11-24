#' Render a coverage value box
#'
#' Shows the coverage of a table, colored by how complete it is.
#'
#' @param .data Data frame or database table (`tbl_dbi`) to measure.
#' @param title Character. Value box title.
#' @param icon Character. Name of a Bootstrap icon, as in
#'   [bsicons::bs_icon()]. Default `"table"`.
#'
#' @returns A [shiny::renderUI()] producing a [bslib::value_box()], colored
#'   `"danger"` below 50%, `"warning"` from 50% to below 80%, and `"success"`
#'   from 80%.
#'
#' @importFrom shiny renderUI
#' @importFrom bslib value_box
#' @importFrom dplyr case_when
#' @importFrom bsicons bs_icon
#' @importFrom govhr compute_global_coverage
#'
#' @details Coverage is the share of cells in the table that are not missing,
#'   as computed by [govhr::compute_global_coverage()]. For example, a table
#'   with 10 rows and 10 columns has 100 cells; if 10 of them hold a value, its
#'   coverage is 10%.
#'
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

#' Render a consistency value box
#'
#' Shows the consistency of a table, colored by how consistent it is, with its
#' record and value consistency underneath.
#'
#' @param .data Data frame or database table (`tbl_dbi`) to measure.
#' @param id_col Character. Column identifying each entity, such as
#'   `"personnel_id"` or `"contract_id"`.
#' @param value_cols Character vector. Columns whose value should stay the
#'   same for each identifier, such as `"birth_date"`.
#' @inheritParams render_coverage_box
#'
#' @returns A [shiny::renderUI()] producing a [bslib::value_box()], colored
#'   `"danger"` below 50%, `"warning"` from 50% to below 80%, and `"success"`
#'   from 80%.
#'
#' @importFrom shiny renderUI p
#' @importFrom bslib value_box
#' @importFrom bsicons bs_icon
#' @importFrom dplyr case_when
#' @importFrom purrr map_dbl
#' @importFrom dplyr pull
#' @importFrom govhr compute_global_consistency compute_record_consistency
#'   compute_value_consistency
#'
#' @details Consistency is the average of two shares, as computed by
#'   [govhr::compute_global_consistency()]:
#'
#'   * Record consistency: the share of identifiers that appear in exactly one
#'     record on each reference date.
#'   * Value consistency: the share of identifiers that keep a single value of
#'     each column in `value_cols`, averaged across those columns.
#'
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

#' Render a validation value box
#'
#' Shows the share of checks a table passes across all validation rules,
#' colored by how many pass.
#'
#' @param validation_data Data frame. The validation report, with one row per
#'   rule and `Passes` and `Total Records` columns, as in the report returned by
#'   [govhr::validate_data()].
#' @inheritParams render_coverage_box
#'
#' @returns A [shiny::renderUI()] producing a [bslib::value_box()], colored
#'   `"danger"` below 50%, `"warning"` from 50% to below 80%, and `"success"`
#'   from 80%.
#'
#' @importFrom shiny renderUI
#' @importFrom bslib value_box
#' @importFrom bsicons bs_icon
#' @importFrom dplyr case_when
#'
#' @export
render_validation_box <- function(validation_data, title, icon = "table") {
  shiny::renderUI({
    total_passes <- sum(validation_data[["Passes"]], na.rm = TRUE)
    total_records <- sum(validation_data[["Total Records"]], na.rm = TRUE)

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