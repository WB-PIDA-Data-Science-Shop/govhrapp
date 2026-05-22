#############################################################################################
################################# VALIDATION MODULE UI #######################################
#############################################################################################

#' Validation Rules UI Module
#'
#' UI components for the Validation Rules section of the quality control dashboard.
#'
#' @param id Character string. The module namespace ID.
#'
#' @return A Shiny UI object containing validation rule cards.
#'
#' @importFrom shiny NS tagList textOutput icon selectInput downloadButton p tags
#' @importFrom bslib layout_columns card card_header card_body value_box
#' @importFrom gt gt_output
#'
#' @keywords internal
validation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(

    # Summary value boxes
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::value_box(
        title    = "Contract Rules Pass Rate",
        value    = shiny::textOutput(ns("contract_pass_rate")),
        showcase = shiny::icon("file-contract"),
        theme    = "primary"
      ),
      bslib::value_box(
        title    = "Personnel Rules Pass Rate",
        value    = shiny::textOutput(ns("personnel_pass_rate")),
        showcase = shiny::icon("users"),
        theme    = "primary"
      )
    ),

    # Contract rules table
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Contract Validation Rules"),
        bslib::card_body(
          gt::gt_output(ns("contract_table")),
          shiny::tags$hr(),
          bslib::layout_columns(
            col_widths = c(8, 4),
            shiny::selectInput(
              ns("contract_rule_select"),
              label    = "Download violations for rule:",
              choices  = NULL
            ),
            shiny::tags$div(
              style = "padding-top: 1.7em;",
              shiny::downloadButton(ns("contract_download"), "Download (.xlsx)")
            )
          )
        )
      )
    ),

    # Personnel rules table
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Personnel Validation Rules"),
        bslib::card_body(
          gt::gt_output(ns("personnel_table")),
          shiny::tags$hr(),
          bslib::layout_columns(
            col_widths = c(8, 4),
            shiny::selectInput(
              ns("personnel_rule_select"),
              label    = "Download violations for rule:",
              choices  = NULL
            ),
            shiny::tags$div(
              style = "padding-top: 1.7em;",
              shiny::downloadButton(ns("personnel_download"), "Download (.xlsx)")
            )
          )
        )
      )
    )
  )
}
