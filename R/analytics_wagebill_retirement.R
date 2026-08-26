

#' Wagebill retirement UI module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#' 
#' @import bslib
#' @import shiny
#' @importFrom plotly plotlyOutput
#' 
#' @return A Shiny module UI function for the wagebill retirement module.
wagebill_retirement_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
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
        shiny::NS(id, "wagebill_measure"),
        "Type of Wage:",
        choices = identify_wagebill_choices(.data),
        selected = "gross_salary_lcu"
      ),
      shiny::actionButton(
        shiny::NS(id, "apply_btn"),
        "Apply selection",
        icon = shiny::icon("play")
      )
    ),
    # plot 1. retirement costs
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Retirement Costs",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Retirement costs over time. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_retirement"),
        height = "350px"
      )
    ),
    # plot 2. projected retirement costs
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Projected Retirement Costs",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Projected retirement costs over time. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_retirement_projection"),
        height = "350px"
      )
    )
  )
}

#' Server for the wagebill retirement module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#' 
#' @import shiny
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom dplyr filter rename
#' @importFrom govhr project_retirement
#' 
#' @return A Shiny module server function for the wagebill retirement module.
wagebill_retirement_server <- function(id, .data) {
  shiny::moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    wagebill_filtered <- shiny::reactive({
      filter_data(
        .data,
        group_filter = input$group_filter,
        subgroup_filter = input$subgroup_filter,
        date_range = input$date_range
      )
    })

    # plot 1. retirement costs
    output$wagebill_retirement <- plotly::renderPlotly({
      retirement_data <- govhr::compute_movement_cost(
        wagebill_filtered(),
        event_type = "retirement",
        measure_col = input$wagebill_measure,
        group_cols = input$group_filter
      )

      plotly::ggplotly(
        plot_trend(
          retirement_data,
          group = input$group_filter,
          y_col = "movement_cost",
          y_label = "Retirement Costs"
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. projected retirement costs
    output$wagebill_retirement_projection <- plotly::renderPlotly({
      retirement_projection_data <- govhr::project_retirement(
        wagebill_filtered(),
        group_cols = input$group_filter,
        measure_col = input$wagebill_measure
      ) |>
        rename(
          ref_date = retirement_date
        )

      plotly::ggplotly(
        plot_trend(
          retirement_projection_data,
          group = input$group_filter,
          y_col = "projected_cost",
          y_label = "Projected Retirement Costs"
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}