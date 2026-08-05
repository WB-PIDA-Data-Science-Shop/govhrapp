
#' Function to create the UI for the wagebill movement module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#' 
#' @import bslib
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom bsicons bs_icon
#' 
#' @return A Shiny module UI function for the wagebill movement module.
wagebill_movement_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = span("Controls", bsicons::bs_icon("sliders")),
      width = "300px",
      !!!ui_filter_controls(.data, id),
      shiny::selectInput(
        shiny::NS(id, "event_type"),
        "Type of Movement:",
        choices = c("Hire" = "hire", "Fire" = "fire"),
        selected = "hire"
      ),
      shiny::selectInput(
        shiny::NS(id, "wagebill_measure"),
        "Type of Wage:",
        choices = identify_wagebill_choices(.data)
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
    # plot 1. labor movement costs
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Labor Movement Costs",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Labor movement costs over time. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_movement"),
        height = "350px"
      )
    ),
    # plot 2. labor movement costs by group
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Labor Movement Costs by Group",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Labor movement costs by group. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_movement_by_group"),
        height = "350px"
      )
    ),
    # plot 3. growth in labor movement costs by group
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Growth in Labor Movement Costs by Group",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Growth in labor movement costs by group. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_movement_growth"),
        height = "350px"
      )
    )
  )
}

#' Function to create the server logic for the wagebill movement module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#' 
#' @import shiny
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom dplyr filter
#' 
#' @return A Shiny module server function for the wagebill movement module.
wagebill_movement_server <- function(id, .data) {
  shiny::moduleServer(id, function(input, output, session) {
    # choice of cols
    wagebill_group_choices <- identify_group_choices(.data)

    update_group_filter_controls(.data, input, session)

    wagebill_filtered <- shiny::reactive({
      data <- .data

      if (input$group_filter != "ref_date") {
        data <- data |>
          dplyr::filter(
            .data[[input$group_filter]] %in% input$subgroup_filter
          )
      }

      data |>
        dplyr::filter(
          .data[["ref_date"]] >= input$date_range[1],
          .data[["ref_date"]] <= input$date_range[2]
        )
    })

    # plot 1. labor movement costs
    output$wagebill_movement <- plotly::renderPlotly({
      labor_movement_data <- govhr::compute_movement_cost(
        wagebill_filtered(),
        event_type = input$event_type,
        measure_col = input$wagebill_measure,
        group_cols = input$group_filter
      )

      plotly::ggplotly(
        plot_trend(
          labor_movement_data,
          group = input$group_filter,
          toggle_growth = input$toggle_growth,
          y_col = "movement_cost",
          y_label = "Movement Costs"
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. labor movement costs by group
    output$wagebill_movement_by_group <- plotly::renderPlotly({
      validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Please select a group."
        )
      )

      labor_movement_data <- govhr::compute_movement_cost(
        wagebill_filtered(),
        event_type = input$event_type,
        measure_col = input$wagebill_measure,
        group_cols = input$group_filter
      )

      n_groups <- nrow(labor_movement_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_total(
          labor_movement_data,
          group = input$group_filter,
          x_col = "movement_cost",
          x_label = "Movement Costs"
        ),
        height = plot_height
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth in labor movement costs by group
    output$wagebill_movement_growth <- plotly::renderPlotly({
      validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Please select a group."
        )
      )

      wagebill_movement_data <- govhr::compute_movement_cost(
        wagebill_filtered(),
        event_type = input$event_type,
        measure_col = input$wagebill_measure,
        group_cols = input$group_filter
      )

      # compute growth between min and max ref_date for each group
      wagebill_movement_growth_data <- wagebill_movement_data[
        ref_date %in% range(ref_date),
        .(
          growth_rate = (movement_cost[ref_date == max(ref_date)] -
            movement_cost[ref_date == min(ref_date)]) /
            movement_cost[ref_date == min(ref_date)]
        ),
        by = c(input$group_filter)
      ]

      n_groups <- nrow(wagebill_movement_growth_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_growth(
          wagebill_movement_growth_data,
          group = input$group_filter
        ),
        height = plot_height
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}