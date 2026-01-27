#############################################################################################
################################# VOLATILITY MODULE UI #######################################
#############################################################################################

#' Volatility UI Module
#'
#' UI components for the Volatility Analysis section of the quality control dashboard.
#'
#' @param id Character string. The module namespace ID.
#'
#' @return A Shiny UI object containing volatility analysis cards.
#'
#' @import shiny bslib
#' @importFrom highcharter highchartOutput
#'
#' @keywords internal
volatility_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    # Introduction card
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_body(
          shiny::markdown(
            "### Volatility Analysis
            
This section evaluates temporal stability across key HR metrics--salary aggregates, contract counts, and work hours. Volatility is measured using period-to-period percentage changes to identify contracts and establishments with unstable patterns over time.

**High volatility may indicate:**
- Data quality issues (inconsistent reporting, missing periods)
- Legitimate organizational changes (restructuring, budget changes)
- Seasonal patterns or policy changes

Use the controls to select which indicators to display and how many top volatile entities to show."
          )
        )
      )
    ),
    
    # Salary Volatility Card
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Contract Salary Volatility (% Change)"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::selectInput(
              ns("salary_indicator"),
              "Salary Indicator:",
              choices = NULL,  # Will be populated dynamically
              selected = NULL
            ),
            shiny::sliderInput(
              ns("salary_top_n"),
              "Number of Contracts to Display:",
              min = 10,
              max = 100,
              value = 50,
              step = 5
            )
          ),
          shiny::p("Top contracts with highest salary volatility. Higher percentage changes indicate greater period-to-period instability."),
          highcharter::highchartOutput(ns("salary_heatmap"), height = "700px")
        )
      )
    ),
    
    # Contract Count Volatility Card
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Contract Count Volatility (% Change)"),
        bslib::card_body(
          shiny::sliderInput(
            ns("contract_count_top_n"),
            "Number of Establishments to Display:",
            min = 10,
            max = 100,
            value = 50,
            step = 5
          ),
          shiny::p("Establishments with highest volatility in contract counts over time."),
          highcharter::highchartOutput(ns("contract_count_heatmap"), height = "700px")
        )
      )
    ),
    
    # Work Hours Volatility Card
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Work Hours Volatility (% Change)"),
        bslib::card_body(
          shiny::sliderInput(
            ns("workhours_top_n"),
            "Number of Contracts to Display:",
            min = 10,
            max = 100,
            value = 50,
            step = 5
          ),
          shiny::p("Contracts with highest volatility in reported work hours."),
          highcharter::highchartOutput(ns("workhours_heatmap"), height = "700px")
        )
      )
    )
  )
}
