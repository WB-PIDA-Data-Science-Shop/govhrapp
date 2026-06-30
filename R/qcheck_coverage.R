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
coverage_ui <- function(id, est_data, contract_data, personnel_data) {
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

  bslib::layout_columns(
    fillable = FALSE,
    bslib::navset_underline(
      bslib::nav_panel(
        title = "Overview",
        bslib::layout_sidebar(
          fillable = FALSE,
          sidebar = bslib::sidebar(
            title = "Controls",
            width = "300px",
            shiny::selectInput(
              shiny::NS(id, "coverage_group"),
              "Group by",
              choices = identify_group_choices(contract_data),
              selected = "All"
            ),
            shinyWidgets::materialSwitch(
              shiny::NS(id, "toggle_growth"),
              label = "Switch to baseline index",
              value = FALSE
            ),
            shiny::actionButton(
              shiny::NS(id, "apply_btn"),
              "Apply selection",
              icon = shiny::icon("play")
            )
          ),
          # plot 1. overall coverage over time
           bslib::card(
            full_screen = TRUE,
            fillable = FALSE,
            bslib::card_header(
              "Coverage over time",
              bslib::tooltip(
                bsicons::bs_icon("info-circle"),
                "Coverage, by year. Choosing a group will add new coverages, by group."
              )
            ),
            plotly::plotlyOutput(
              shiny::NS(id, "coverage_panel"),
              height = "350px"
            )
          )
        )
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

    # 2. coverage plots
    data_coverage_overall <- shiny::reactive({
      compute_coverage(contract_data, group = input$coverage_group, aggregate = TRUE)
    })

    output$coverage_panel <- plotly::renderPlotly({
      plot_trend(
        data_coverage_overall(),
        y_col = "coverage",
        group = input$coverage_group,
        toggle_growth = input$toggle_growth,
        y_label = "Wage Bill"
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
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
