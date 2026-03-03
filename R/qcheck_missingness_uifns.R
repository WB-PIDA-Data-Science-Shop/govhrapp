#############################################################################################
################################# MISSINGNESS MODULE UI & SERVER #############################
#############################################################################################

#' Missingness UI Module
#'
#' UI components for the Missingness section of the quality control dashboard.
#'
#' @param id Character string. The module namespace ID.
#'
#' @return A Shiny UI object containing missingness analysis cards.
#'
#' @import shiny bslib
#' @importFrom highcharter highchartOutput
#' @importFrom gt gt_output
#'
#' @keywords internal
missingness_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(12),
      
      # Card 1: Overall Missingness
      bslib::card(
        bslib::card_header("Overall Missingness by Variable"),
        bslib::card_body(
          shiny::p("Variables with missing data across all modules. Higher percentages indicate potential data quality issues."),
          highcharter::highchartOutput(ns("overall_plot"), height = "600px")
        )
      )
    ),
    
    bslib::layout_columns(
      col_widths = c(6, 6),
      
      # Card 2: Missingness by Occupation
      bslib::card(
        bslib::card_header("Missingness by Occupation (Top 15)"),
        bslib::card_body(
          shiny::p("Missingness patterns across the most affected occupations."),
          highcharter::highchartOutput(ns("occupation_plot"), height = "500px")
        )
      ),
      
      # Card 3: Missingness by ISCO
      bslib::card(
        bslib::card_header("Missingness by ISCO Code (Top 15)"),
        bslib::card_body(
          shiny::p("Missingness patterns across standardized occupation classifications."),
          highcharter::highchartOutput(ns("isco_plot"), height = "500px")
        )
      )
    ),
    
    bslib::layout_columns(
      col_widths = c(12),
      
      # Card 4: Missingness Heatmap by Reference Date
      bslib::card(
        bslib::card_header("Missingness Heatmap by Reference Date"),
        bslib::card_body(
          shiny::p("Temporal patterns in missing data. Darker colors indicate higher missingness."),
          highcharter::highchartOutput(ns("ref_date_heatmap"), height = "500px")
        )
      )
    ),
    
    bslib::layout_columns(
      col_widths = c(12),
      
      # Card 5: Missingness by Establishment
      bslib::card(
        bslib::card_header("Missingness by Establishment (Top 15)"),
        bslib::card_body(
          shiny::p("Establishments with the highest missingness rates."),
          highcharter::highchartOutput(ns("establishment_plot"), height = "500px")
        )
      )
    )
  )
}


