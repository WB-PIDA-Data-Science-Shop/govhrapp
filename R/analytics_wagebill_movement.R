#' Wage bill movement panel UI
#'
#' Sidebar controls and plots of the cost of hires or separations, over time and
#' by group.
#'
#' @param id Character. Module namespace ID.
#' @param .data Contract data, as `wagebill_data` in
#'   [run_govhrapp_analytics()].
#'
#' @returns A Shiny UI definition.
#'
#' @import bslib
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom plotly plotlyOutput
#' @keywords internal
wagebill_movement_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!default_ui_controls(id, .data),
      breakdown_filter_ui(id, .data),
      shiny::selectInput(
        shiny::NS(id, "event_type"),
        "Movement",
        choices = c("Hires" = "hire", "Separations" = "separation"),
        selected = "hire"
      ),
      shiny::selectInput(
        shiny::NS(id, "wagebill_measure"),
        "Pay measure",
        choices = identify_wagebill_choices(.data)
      ),
      shinyWidgets::materialSwitch(
        shiny::NS(id, "toggle_growth"),
        label = "Show as index (first date = 100)",
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
        "Movement cost over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Pay of hires on the date they were hired, or of separations on their last active date, added up for each reference date. Select a group to compare subgroups.",
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
        "Movement cost by group",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Movement cost added up over the selected date range, by subgroup.",
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
        "Growth in movement cost by group",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Percentage change in movement cost from the first to the last reference date with movers in the selected date range, by subgroup.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_movement_growth"),
        height = "350px"
      )
    ),
    composition_ui(shiny::NS(id, "composition"))
  )
}

#' Wage bill movement panel server
#'
#' Renders the movement cost plots, from the cache until the user applies a
#' selection.
#'
#' @inheritParams wagebill_movement_ui
#' @param cache List of precomputed summaries from [build_analytics_cache()].
#'
#' @returns Called for its side effect of rendering the module's outputs.
#'
#' @import shiny
#' @importFrom dplyr collect filter
#' @importFrom govhr compute_growth compute_movement_cost plot_bar_growth plot_bar_total plot_trend scale_plot_height
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom purrr pluck
#' @keywords internal
wagebill_movement_server <- function(id, .data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    update_filter_controls(.data, input, session)

    wagebill_filtered <- shiny::reactive({
      apply_sidebar_filters(.data, input)
    })

    # all three plots read the same aggregate, so it is computed and collected
    # once per apply. govhr's grouped plot_trend(), scale_plot_height() and
    # compute_growth() also need it in memory
    movement_cost <- shiny::reactive({
      cost <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "wagebill", "wagebill_movement")
      } else {
        govhr::compute_movement_cost(
          wagebill_filtered(),
          event_type = input$event_type,
          measure_col = input$wagebill_measure,
          group_cols = group_col_to_null(input$group_filter)
        ) |>
          dplyr::collect()
      }

      # the cost is NA on the first date, which has nothing to compare with,
      # and for separations from a group with nobody active on the previous date
      dplyr::filter(cost, !is.na(.data[["movement_cost"]]))
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
          y_label = "Movement cost"
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. labor movement costs by group
    output$wagebill_movement_by_group <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Select a group to see this chart."
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
          x_label = "Movement cost"
        ),
        height = govhr::scale_plot_height(movement_cost_data)
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth in labor movement costs by group
    output$wagebill_movement_growth <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Select a group to see this chart."
        )
      )

      # growth from a date without movers is infinite, so each group's growth
      # runs between the first and last dates it had movers
      movement_cost_growth <- movement_cost() |>
        dplyr::filter(.data[["movement_cost"]] > 0) |>
        govhr::compute_growth(
          group_col = input$group_filter,
          measure_col = "movement_cost"
        )

      plotly::ggplotly(
        govhr::plot_bar_growth(
          movement_cost_growth,
          group_col = input$group_filter
        ),
        height = govhr::scale_plot_height(movement_cost_growth)
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    composition_server(
      "composition",
      filtered = \() wagebill_filtered(),
      group_col = \() input$group_filter,
      breakdown_col = \() input$breakdown_filter,
      apply = \() input$apply_btn,
      measure = \() list(
        compute = \(.data, group_cols) {
          govhr::compute_movement_cost(
            .data,
            event_type = input$event_type,
            measure_col = input$wagebill_measure,
            group_cols = group_cols
          )
        },
        value_col = "movement_cost",
        label = paste("cost of", tolower(movement_labels[[input$event_type]]))
      )
    )
  })
}
