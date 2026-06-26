
#' coverage UI Module
#'
#' UI components for the coverage section of the quality control dashboard.
#'
#' @param id Character string. The module namespace ID.
#' @param est_data Data frame. The establishment-level data.
#' @param contract_data Data frame. The contract-level data.
#' @param personnel_data Data frame. The personnel-level data.
#'
#' @return A Shiny UI object containing coverage analysis cards.
#'
#' @import shiny
#' @import bslib
#'
#' @keywords internal
coverage_ui <- function(id,  est_data, contract_data, personnel_data) {
  # coverage per module
  value_boxes <- list(
    uiOutput(
      NS(id, "coverage_est")
    ),
    uiOutput(
      NS(id, "coverage_contract")
    ),
    uiOutput(
      NS(id, "coverage_personnel")
    )
  )

  bslib::page_fillable(
    # 1. value boxes for coverage metrics
    bslib::card(
      bslib::card_header(
        "Data Coverage"
      ),
      layout_column_wrap(
        fill = FALSE,
        !!!value_boxes
      )
    )
  )
}

#' coverage Server Module
#'
#' Server logic for the coverage section, processing and visualizing missing data patterns.
#'
#' @param id Character string. The module namespace ID.
#' @param est_data Data frame. The establishment-level data.
#' @param contract_data Data frame. The contract-level data.
#' @param personnel_data Data frame. The personnel-level data.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @import shiny
#'
#' @keywords internal
coverage_server <- function(id, est_data, contract_data, personnel_data) {
  shiny::moduleServer(id, function(input, output, session) {
    output$coverage_est <- render_coverage_box(est_data, "Establishments", "building")

    output$coverage_personnel <- render_coverage_box(personnel_data, "Personnel", "users")
    
    output$coverage_contract <- render_coverage_box(contract_data, "Contracts", "file-contract")
  })
}

run_coverageapp <- function(
  est_data,
  personnel_data,
  contract_data,
  ...
) {
  ui <- coverage_ui("test", est_data, contract_data, personnel_data)

  server <- function(input, output, session) {
    coverage_server("test", est_data, contract_data, personnel_data)
  }

  shiny::shinyApp(ui, server, ...)
}
