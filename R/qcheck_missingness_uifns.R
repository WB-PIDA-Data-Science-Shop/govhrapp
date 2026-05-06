#############################################################################################
################################# MISSINGNESS MODULE UI ######################################
#############################################################################################

#' Missingness UI Module
#'
#' UI components for the Missingness section of the quality control dashboard.
#'
#' @param id Character string. The module namespace ID.
#'
#' @return A Shiny UI object containing missingness analysis cards.
#'
#' @importFrom shiny NS tagList markdown selectInput sliderInput p
#' @importFrom bslib layout_columns card card_header card_body
#' @importFrom highcharter highchartOutput
#'
#' @keywords internal
missingness_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(

    # Module selector (shared between both cards)
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(3, 9),
            shiny::selectInput(
              ns("module"),
              "Module:",
              choices = NULL
            ),
            shiny::markdown(
              "### Data Coverage Analysis

This section shows missing data rates across key fields.
Select a module above to switch between contract and personnel fields.
**Card 1** shows the overall rate per field. **Card 2** breaks it down by a grouping variable."
            )
          )
        )
      )
    ),

    # Card 1: Overall missingness bar chart
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Overall Missingness by Field"),
        bslib::card_body(
          shiny::p("Overall missingness rate per field. Fields with 0% missing are hidden."),
          highcharter::highchartOutput(ns("overall_bar"), height = "300px")
        )
      )
    ),

    # Card 2: Grouped heatmap
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Missingness by Group"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::selectInput(
              ns("group_var"),
              "Group By:",
              choices = NULL
            ),
            shiny::sliderInput(
              ns("top_n"),
              "Top N groups to show:",
              min = 5, max = 50, value = 20, step = 5
            ),
            shiny::selectInput(
              ns("sort_by"),
              "Sort groups by:",
              choices = c(
                "Highest overall missingness" = "desc",
                "Lowest overall missingness"  = "asc",
                "Alphabetical"                = "alpha"
              )
            )
          ),
          shiny::p(
            "Rows = fields, Columns = group values.",
            "Darker red = higher missingness."
          ),
          highcharter::highchartOutput(ns("missingness_heatmap"), height = "500px")
        )
      )
    )
  )
}
