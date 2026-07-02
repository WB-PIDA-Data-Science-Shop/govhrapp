#' Consistency UI Module
#'
#' UI components for the consistency section of the quality control dashboard.
#'
#' @param id Character string. The module namespace ID.
#' @param est_data Data frame. The establishment-level data.
#' @param personnel_data Data frame. The personnel-level data.
#' @param contract_data Data frame. The contract-level data.
#'
#' @return A Shiny UI object containing coverage analysis cards.
#'
#' @import shiny
#' @import bslib
#'
#' @keywords internal
coverage_ui <- function(id, est_data, personnel_data, contract_data) {

}


#' Consistency Server Module
#'
#' Server logic for the consistency section, processing and visualizing whether data is consistent.
#'
#' @param id Character string. The module namespace ID.
#' @param est_data Data frame. The establishment-level data.
#' @param personnel_data Data frame. The personnel-level data.
#' @param contract_data Data frame. The contract-level data.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @import shiny
#'
#' @keywords internal
coverage_server <- function(id, est_data, personnel_data, contract_data) {
  shiny::moduleServer(id, function(input, output, session) {
    # 1. value boxes for coverage metrics
    output$coverage_est <- render_coverage_box(
      est_data,
      "Establishments",
      "building"
    )
    output$coverage_personnel <- render_coverage_box(
      personnel_data,
      "Personnel",
      "users"
    )
    output$coverage_contract <- render_coverage_box(
      contract_data,
      "Contracts",
      "file-contract"
    )

    # per-dataset panel sub-modules
    coverage_panel_server("est",       est_data)
    coverage_panel_server("personnel", personnel_data)
    coverage_panel_server("contract",  contract_data)
  })
}