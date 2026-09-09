#' Wage Bill Movement UI
#'
#' Sidebar controls and plots for the cost of recruitment and separation.
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame containing wage bill data.
#'
#' @return A Shiny UI definition.
#'
#' @import bslib
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom plotly plotlyOutput
wagebill_movement_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!default_ui_controls(.data, id),
      shiny::selectInput(
        shiny::NS(id, "event_type"),
        "Type of Movement:",
        choices = c("Recruitment" = "hire", "Separation" = "fire"),
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

#' Wage Bill Movement Server
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame containing wage bill data.
#' @param cache List of pre-computed summaries from [build_analytics_cache()].
#'
#' @return A Shiny module server function.
#'
#' @import shiny
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom purrr pluck
#' @keywords internal
wagebill_movement_server <- function(id, .data, cache) {
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

    # all three plots read the same aggregate, so compute it once per apply
    movement_cost <- shiny::reactive({
      if (input$apply_btn == 0) {
        purrr::pluck(cache, "wagebill", "wagebill_movement")
      } else {
        govhr::compute_movement_cost(
          wagebill_filtered(),
          event_type = input$event_type,
          measure_col = input$wagebill_measure,
          group_cols = input$group_filter
        )
      }
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 1. labor movement costs
    output$wagebill_movement <- plotly::renderPlotly({
      plotly::ggplotly(
        govhr::plot_trend(
          movement_cost(),
          group_col = input$group_filter,
          toggle_growth = input$toggle_growth,
          y_col = "movement_cost",
          y_label = "Movement Costs"
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. labor movement costs by group
    output$wagebill_movement_by_group <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Please select a group."
        )
      )

      movement_cost_data <- movement_cost() |>
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(input$group_filter)
          )
        ) |>
        summarise(
          movement_cost = sum(movement_cost, na.rm = TRUE),
          .groups = "drop"
        )

      plotly::ggplotly(
        govhr::plot_bar_total(
          movement_cost_data,
          group_col = input$group_filter,
          x_col = "movement_cost",
          x_label = "Movement Costs"
        ),
        height = scale_plot_height(movement_cost_data)
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth in labor movement costs by group
    output$wagebill_movement_growth <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Please select a group."
        )
      )

      # growth between the first and last reference date, by group
      movement_cost_growth <- movement_cost()[
        ref_date %in% range(ref_date),
        .(
          growth_rate = (movement_cost[ref_date == max(ref_date)] -
            movement_cost[ref_date == min(ref_date)]) /
            movement_cost[ref_date == min(ref_date)]
        ),
        by = c(input$group_filter)
      ]

      plotly::ggplotly(
        govhr::plot_bar_growth(
          movement_cost_growth,
          group_col = input$group_filter
        ),
        height = scale_plot_height(movement_cost_growth)
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}
