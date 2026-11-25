#' Workforce retirement panel UI
#'
#' Sidebar controls and plots of pensioners and of past and projected
#' retirements.
#'
#' @param id Character. Module namespace ID.
#' @param .data Personnel data, as `workforce_data` in
#'   [run_govhrapp_analytics()].
#'
#' @returns A Shiny UI definition.
#'
#' @import bslib
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @keywords internal
workforce_retirement_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    theme = bslib::bs_theme(bootswatch = "litera"),
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
        shiny::NS(id, "measurement_type"),
        label = "Measure",
        choices = c("Count" = "count", "Rate" = "rate")
      ),
      shiny::actionButton(
        shiny::NS(id, "apply_btn"),
        "Apply selection",
        icon = shiny::icon("play")
      )
    ),

    # plot 1. pensioners over time
    bslib::card(
      bslib::card_header(
        "Pensioners over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Number of personnel recorded as pensioners on each reference date. Select a group to compare subgroups.",
          title = "Pensioners over time",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "pensioner_plot"))
    ),

    # plot 2. retirement counts/rates over time
    bslib::card(
      bslib::card_header(
        "Retirements over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Personnel active on the previous reference date who are recorded as pensioners on this one. The rate divides retirements by the previous reference date's active headcount. Select a group to compare subgroups.",
          title = "Retirements over time",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "retirement_plot"))
    ),

    # plot 3. projected retirements
    bslib::card(
      bslib::card_header(
        "Projected retirements",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Personnel active on the latest reference date who reach the retirement age in each of the next 10 years. The rate divides them by that date's active headcount. Select a group to compare subgroups.",
          title = "Projected retirements",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "retirement_expected_plot"))
    ),
    composition_ui(shiny::NS(id, "composition"))
  )
}

#' Workforce retirement panel server
#'
#' Renders the retirement plots, from the cache until the user applies a
#' selection.
#'
#' @inheritParams workforce_retirement_ui
#' @param cache List of precomputed summaries from [build_analytics_cache()].
#'
#' @returns Called for its side effect of rendering the module's outputs.
#'
#' @import shiny
#' @importFrom dplyr collect
#' @importFrom govhr compute_headcount compute_retirement project_retirement
#' @importFrom plotly renderPlotly
#' @importFrom purrr pluck
#' @keywords internal
workforce_retirement_server <- function(id, .data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    update_filter_controls(.data, input, session)

    data_filtered <- shiny::reactive({
      shiny::req(input$apply_btn)

      apply_sidebar_filters(.data, input)
    })

    # plot 1. pensioners over time
    output$pensioner_plot <- plotly::renderPlotly({
      plot_data <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "workforce", "workforce_pensioners")
      } else {
        data_filtered() |>
          filter_pensioners() |>
          govhr::compute_headcount(
            group_cols = group_col_to_null(input$group_filter)
          ) |>
          dplyr::collect()
      }

      # pensioner records can lack contract details, so a contract grouping
      # may leave none, and plotly cannot draw an empty chart
      shiny::validate(
        shiny::need(
          nrow(plot_data) > 0,
          "No pensioners for the selected groups and dates."
        )
      )

      plot_movement_trend(
        plot_data,
        y_col = "headcount",
        y_label = "Pensioners",
        group_col = input$group_filter
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. retirement counts/rates over time
    output$retirement_plot <- plotly::renderPlotly({
      plot_data <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "workforce", "workforce_retirement")
      } else {
        govhr::compute_retirement(
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

    # plot 3. projected retirements
    output$retirement_expected_plot <- plotly::renderPlotly({
      plot_data <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "workforce", "workforce_retirement_expected")
      } else {
        govhr::project_retirement(
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

    composition_server(
      "composition",
      filtered = \() filter_pensioners(data_filtered()),
      group_col = \() input$group_filter,
      breakdown_col = \() input$breakdown_filter,
      apply = \() input$apply_btn,
      measure = \() list(
        compute = govhr::compute_headcount,
        value_col = "headcount",
        label = "pensioners"
      )
    )
  })
}
