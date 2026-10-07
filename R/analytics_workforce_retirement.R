#' Workforce Retirement UI
#'
#' Sidebar controls and plots for realised and projected retirements.
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame containing personnel data.
#'
#' @return A Shiny UI definition.
#'
#' @import bslib
#' @import shiny
#' @importFrom plotly plotlyOutput
workforce_retirement_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    theme = bslib::bs_theme(bootswatch = "litera"),
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!default_ui_controls(.data, id),
      shiny::numericInput(
        shiny::NS(id, "threshold_age"),
        label = "Select retirement threshold age:",
        value = 60,
        min = 50,
        max = 70
      ),
      shiny::selectInput(
        shiny::NS(id, "measurement_type"),
        label = "Select type of measurement:",
        choices = c("Count" = "count", "Rate" = "rate")
      ),
      shiny::actionButton(
        shiny::NS(id, "apply_btn"),
        "Apply selection",
        icon = shiny::icon("play")
      )
    ),

    # plot 1. retirement counts/rates over time
    bslib::card(
      bslib::card_header(
        "Retirements over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Retirements are personnel active in a period who are pensioners in the next one. The rate divides retirements by the active headcount in the same period.",
          title = "Retirements over time",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "retirement_plot"))
    ),

    # plot 2. projected retirements
    bslib::card(
      bslib::card_header(
        "Projected retirements",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Active personnel in the latest period who reach the selected retirement threshold age in each future year. The rate divides them by the active headcount in the latest period.",
          title = "Projected retirements",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "retirement_expected_plot"))
    )
  )
}

#' Workforce Retirement Server
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame containing personnel data.
#' @param cache List of pre-computed summaries from [build_analytics_cache()].
#'
#' @return A Shiny module server function.
#'
#' @import shiny
#' @importFrom plotly renderPlotly
#' @importFrom purrr pluck
#' @keywords internal
workforce_retirement_server <- function(id, .data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    data_filtered <- shiny::reactive({
      shiny::req(input$apply_btn)

      filter_data(
        .data,
        group_filter = input$group_filter,
        subgroup_filter = input$subgroup_filter,
        date_range = input$date_range
      )
    })

    # plot 1. retirement counts/rates over time
    output$retirement_plot <- plotly::renderPlotly({
      plot_data <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "workforce", "workforce_retirement")
      } else {
        compute_retirement(
          data_filtered(),
          group_cols = group_col_to_null(input$group_filter)
        )
      }

      plot_movement_trend(
        plot_data,
        y_col = movement_measure_col("retirement", input$measurement_type),
        y_label = if (input$measurement_type == "count") {
          "Retirements"
        } else {
          "Retirement rate"
        },
        group_col = input$group_filter,
        percent = input$measurement_type == "rate"
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. projected retirements
    output$retirement_expected_plot <- plotly::renderPlotly({
      plot_data <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "workforce", "workforce_retirement_expected")
      } else {
        project_retirement(
          data_filtered(),
          threshold_age = input$threshold_age,
          group_cols = group_col_to_null(input$group_filter)
        )
      }

      plot_movement_trend(
        plot_data,
        y_col = if (input$measurement_type == "count") {
          "projected_retirements"
        } else {
          "projected_retirement_rate"
        },
        y_label = if (input$measurement_type == "count") {
          "Projected retirements"
        } else {
          "Projected retirement rate"
        },
        group_col = input$group_filter,
        percent = input$measurement_type == "rate"
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}
