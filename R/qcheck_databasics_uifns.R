#' Data Basics UI Module
#'
#' Creates the UI for the Data Basics section of the quality control dashboard,
#' displaying module dimensions, temporal coverage, variable structure, key integrity,
#' and orphan checks.
#'
#' @param id Character string. The module namespace ID.
#'
#' @return A Shiny UI element containing cards with diagnostic visualizations.
#'
#' @importFrom shiny NS plotOutput uiOutput sliderInput
#' @importFrom bslib layout_columns card card_header card_body
#' @importFrom gt gt_output
#' @importFrom highcharter highchartOutput
#'
#' @keywords internal
databasics_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_columns(
    col_widths = 12,
    
    # Module Dimensions Card
    bslib::card(
      bslib::card_header("Module Dimensions"),
      bslib::card_body(
        gt::gt_output(ns("dimensions_table"))
      )
    ),
    
    # Temporal Coverage Card
    bslib::card(
      bslib::card_header("Temporal and Cross-Sectional Coverage"),
      bslib::card_body(
        shiny::uiOutput(ns("date_slider_ui")),
        highcharter::highchartOutput(ns("coverage_plot"), height = "500px")
      )
    ),
    
    # Variable Structure Card
    bslib::card(
      bslib::card_header("Variable Structure & Dictionary Conformity"),
      bslib::card_body(
        gt::gt_output(ns("structure_table"))
      )
    ),
    
    # Primary Key Integrity Card
    bslib::card(
      bslib::card_header("Primary Key Integrity"),
      bslib::card_body(
        gt::gt_output(ns("keys_table")),
        shiny::uiOutput(ns("duplicate_keys_ui"))
      )
    ),
    
    # Orphan Checks Card
    bslib::card(
      bslib::card_header("Cross-Module Orphan Checks"),
      bslib::card_body(
        gt::gt_output(ns("orphans_table")),
        shiny::uiOutput(ns("orphan_details_ui"))
      )
    )
  )
}
