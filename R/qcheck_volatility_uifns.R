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
#' @importFrom shiny NS tagList markdown selectInput sliderInput p
#' @importFrom bslib layout_columns card card_header card_body
#' @importFrom highcharter highchartOutput
#'
#' @keywords internal
volatility_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(

    # Introduction
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_body(
          shiny::markdown(
            "### Temporal Stability Analysis

This section evaluates period-to-period percentage changes to identify contracts and
establishments with unstable patterns over time.

**High volatility may indicate:**
- Data quality issues (inconsistent reporting, missing periods)
- Legitimate organizational changes (restructuring, budget changes)
- Seasonal patterns or policy changes

Use the controls to select indicators and how many top volatile entities to display."
          )
        )
      )
    ),

    # Contract volatility
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Contract-Level Volatility (% Change)"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::selectInput(
              ns("contract_indicator"),
              "Indicator:",
              choices = NULL
            ),
            shiny::sliderInput(
              ns("contract_top_n"),
              "Number of Contracts to Display:",
              min = 10, max = 100, value = 50, step = 5
            )
          ),
          shiny::p("Top contracts with highest period-to-period volatility. Brighter colors indicate greater instability."),
          highcharter::highchartOutput(ns("contract_heatmap"), height = "700px")
        )
      )
    ),

    # Personnel / headcount volatility
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Headcount Volatility by Establishment (% Change)"),
        bslib::card_body(
          shiny::sliderInput(
            ns("personnel_top_n"),
            "Number of Establishments to Display:",
            min = 10, max = 100, value = 50, step = 5
          ),
          shiny::p("Establishments with the highest volatility in headcount over time."),
          highcharter::highchartOutput(ns("personnel_heatmap"), height = "700px")
        )
      )
    )
  )
}
