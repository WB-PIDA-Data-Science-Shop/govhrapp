#' coverage UI Module
#'
#' UI components for the coverage section of the quality control dashboard.
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

  bslib::layout_columns(
    fillable = FALSE,
    col_widths = 12,

    # 1. value boxes for coverage metrics
    bslib::card(
      bslib::card_header(
        "Data Coverage",
        bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Coverage, by module. Computed as the global average of coverage across all variables in each module."
        )
      ),
      layout_column_wrap(
        fill = FALSE,
        !!!value_boxes
      )
    ),
    
    # 2. coverage by module
    bslib::navset_underline(
      bslib::nav_panel(
        title = "Establishment",
        coverage_panel_ui(NS(id, "est"), est_data)
      ),
      bslib::nav_panel(
        title = "Personnel",
        coverage_panel_ui(NS(id, "personnel"), personnel_data)
      ),
      bslib::nav_panel(
        title = "Contract",
        coverage_panel_ui(NS(id, "contract"), contract_data)
      )
    )
  )
}

#' Coverage Server Module
#'
#' Server logic for the coverage section, processing and visualizing missing data patterns.
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
