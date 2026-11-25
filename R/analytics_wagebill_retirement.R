#' Wage bill retirement panel UI
#'
#' Sidebar controls and plots of pension payments and of past and projected
#' retirement costs.
#'
#' @param id Character. Module namespace ID.
#' @param .data Contract data, as `wagebill_data` in
#'   [run_govhrapp_analytics()].
#'
#' @returns A Shiny UI definition.
#'
#' @import bslib
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @keywords internal
wagebill_retirement_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!default_ui_controls(id, .data),
      breakdown_filter_ui(id, .data),
      shiny::numericInput(
        shiny::NS(id, "threshold_age"),
        label = "Retirement age",
        value = 60,
        min = 50,
        max = 70
      ),
      shiny::selectInput(
        shiny::NS(id, "wagebill_measure"),
        "Pay measure",
        choices = identify_wagebill_choices(.data),
        selected = "gross_salary_lcu"
      ),
      shiny::actionButton(
        shiny::NS(id, "apply_btn"),
        "Apply selection",
        icon = shiny::icon("play")
      )
    ),
    # plot 1. pension payments
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Pension payments over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Total pay of personnel recorded as pensioners on each reference date. Select a group to compare subgroups.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_pensions"),
        height = "350px"
      )
    ),
    # plot 2. retirement costs
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Retirement cost over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Pay of retirees on their last active date, added up for each reference date. Select a group to compare subgroups.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_retirement"),
        height = "350px"
      )
    ),
    # plot 3. projected retirement costs
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Projected retirement cost",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Estimated pension cost of personnel active on the latest reference date who reach the retirement age in each of the next 10 years: their current pay times 0.6. Select a group to compare subgroups.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_retirement_projection"),
        height = "350px"
      )
    ),
    composition_ui(shiny::NS(id, "composition"))
  )
}

#' Wage bill retirement panel server
#'
#' Renders the retirement cost plots, from the cache until the user applies a
#' selection.
#'
#' @inheritParams wagebill_retirement_ui
#' @param cache List of precomputed summaries from [build_analytics_cache()].
#'
#' @returns Called for its side effect of rendering the module's outputs.
#'
#' @import shiny
#' @importFrom dplyr collect
#' @importFrom govhr compute_retirement_cost compute_wagebill project_retirement
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom purrr pluck
#' @keywords internal
wagebill_retirement_server <- function(id, .data, cache) {
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

    # plot 1. pension payments
    output$wagebill_pensions <- plotly::renderPlotly({
      pension_data <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "wagebill", "wagebill_pensions")
      } else {
        wagebill_filtered() |>
          filter_pensioners() |>
          govhr::compute_wagebill(
            measure_col = input$wagebill_measure,
            group_cols = group_col_to_null(input$group_filter)
          ) |>
          dplyr::collect()
      }

      # pensioner records can lack contract details, so a contract grouping
      # may leave none, and plotly cannot draw an empty chart
      shiny::validate(
        shiny::need(
          any(!is.na(pension_data[["wagebill"]])),
          "No pensioners for the selected groups and dates."
        )
      )

      plotly::ggplotly(
        plot_movement_trend(
          pension_data,
          y_col = "wagebill",
          y_label = "Pension payments",
          group_col = input$group_filter
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. retirement costs
    output$wagebill_retirement <- plotly::renderPlotly({
      retirement_data <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "wagebill", "wagebill_retirement")
      } else {
        govhr::compute_retirement_cost(
          wagebill_filtered(),
          measure_col = input$wagebill_measure,
          group_cols = group_col_to_null(input$group_filter)
        )
      }

      plotly::ggplotly(
        plot_movement_trend(
          retirement_data,
          y_col = "retirement_cost",
          y_label = "Retirement cost",
          group_col = input$group_filter
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. projected retirement costs
    output$wagebill_retirement_projection <- plotly::renderPlotly({
      retirement_projection_data <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "wagebill", "wagebill_retirement_expected")
      } else {
        govhr::project_retirement(
          wagebill_filtered(),
          threshold_age = input$threshold_age,
          group_cols = group_col_to_null(input$group_filter),
          measure_col = input$wagebill_measure
        )
      }

      plotly::ggplotly(
        plot_movement_trend(
          retirement_projection_data,
          y_col = "projected_cost",
          y_label = "Projected retirement cost",
          group_col = input$group_filter
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    composition_server(
      "composition",
      filtered = \() filter_pensioners(wagebill_filtered()),
      group_col = \() input$group_filter,
      breakdown_col = \() input$breakdown_filter,
      apply = \() input$apply_btn,
      measure = \() list(
        compute = \(.data, group_cols) {
          govhr::compute_wagebill(
            .data,
            measure_col = input$wagebill_measure,
            group_cols = group_cols
          )
        },
        value_col = "wagebill",
        label = "pension payments"
      )
    )
  })
}
