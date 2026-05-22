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

This section evaluates period-to-period percentage changes to identify entities with
unstable patterns over time.

**High volatility may indicate:**
- Data quality issues (inconsistent reporting, missing periods)
- Legitimate organizational changes (restructuring, budget changes)
- Seasonal patterns or policy changes

Select a **Statistic Type** to define what is being measured, then choose a **Group By**
dimension to slice the data. Use the Top N slider to limit the display to the most volatile entities."
          )
        )
      )
    ),

    # Contract volatility
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Contract Volatility (% Change)"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::selectInput(
              ns("contract_stat_type"),
              "Statistic Type:",
              choices = NULL
            ),
            shiny::selectInput(
              ns("contract_group_var"),
              "Group By:",
              choices = NULL
            ),
            shiny::sliderInput(
              ns("contract_top_n"),
              "Top N Entities:",
              min = 10, max = 100, value = 50, step = 5
            )
          ),
          # Conditional indicator selector (only for salary_vol which has 4 indicators)
          shiny::conditionalPanel(
            condition = paste0("input['" , ns("contract_stat_type"), "'] == 'salary_vol'"),
            shiny::selectInput(
              ns("contract_indicator"),
              "Salary Indicator:",
              choices = NULL
            )
          ),
          shiny::p("Top entities with highest period-to-period volatility. Brighter colours indicate greater instability."),
          highcharter::highchartOutput(ns("contract_heatmap"), height = "700px")
        )
      )
    ),

    # Personnel volatility
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Personnel Volatility (% Change)"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::selectInput(
              ns("personnel_group_var"),
              "Group By:",
              choices = NULL
            ),
            shiny::sliderInput(
              ns("personnel_top_n"),
              "Top N Entities:",
              min = 10, max = 100, value = 50, step = 5
            )
          ),
          shiny::p("Entities with the highest volatility in headcount over time."),
          highcharter::highchartOutput(ns("personnel_heatmap"), height = "700px")
        )
      )
    )
  )
}
