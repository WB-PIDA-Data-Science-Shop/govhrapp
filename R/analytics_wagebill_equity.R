#' Wage bill equity panel UI
#'
#' Sidebar controls and plots of the pay distribution, average pay by decile,
#' and pay compression.
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
wagebill_equity_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!default_ui_controls(id, .data),
      shiny::selectInput(
        shiny::NS(id, "wagebill_measure"),
        "Pay measure",
        choices = identify_wagebill_choices(.data)
      ),
      shiny::radioButtons(
        inputId = shiny::NS(id, "plot_type"),
        label = "Distribution",
        choices = c("Histogram" = "histogram", "Cumulative" = "cumulative"),
        selected = "histogram"
      ),
      shiny::actionButton(
        shiny::NS(id, "apply_btn"),
        "Apply selection",
        icon = shiny::icon("play")
      )
    ),
    # plot 1. wage distribution
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Pay distribution",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Share of active personnel's records in each pay range, pooling every reference date in the selected date range. Select a group to compare subgroups.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_density"),
        height = "350px"
      )
    ),
    # plot 2. wage by decile
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Pay by decile",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Average pay of active personnel in each tenth of the pay distribution, from lowest to highest paid, on the latest reference date. Select a group to compare subgroups.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_distribution"),
        height = "350px"
      )
    ),
    # plot 3. compression ratio
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Pay compression (10th to 90th percentile)",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "The 10th, 50th and 90th percentiles of active personnel's pay. The wider the gap between the 10th and 90th, the less compressed pay is. Without a group, each reference date is shown; with a group, subgroups are compared on the latest reference date.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_compression_ratio"),
        height = "350px"
      )
    )
  )
}

#' Wage bill equity panel server
#'
#' Renders the pay distribution plots, from the cache until the user applies a
#' selection.
#'
#' @inheritParams wagebill_equity_ui
#' @param cache List of precomputed summaries from [build_analytics_cache()].
#'
#' @returns Called for its side effect of rendering the module's outputs.
#'
#' @import shiny
#' @importFrom govhr compute_compression_ratio compute_decile compute_percentile
#' @importFrom plotly renderPlotly
#' @importFrom purrr pluck
#' @keywords internal
wagebill_equity_server <- function(id, .data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    wagebill_filtered <- shiny::reactive({
      filter_data(
        .data,
        group_filter = input$group_filter,
        subgroup_filter = input$subgroup_filter,
        date_range = input$date_range
      ) |>
        filter_active()
    })

    # plot 1. wage density
    output$wagebill_density <- plotly::renderPlotly({
      wagebill_density <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "wagebill", "wagebill_equity_percentile")
      } else {
        govhr::compute_percentile(
          wagebill_filtered(),
          measure_col = input$wagebill_measure,
          group_cols = group_col_to_null(input$group_filter)
        )
      }

      govhr::plot_histogram(
        wagebill_density,
        plot_type = input$plot_type,
        group_col = input$group_filter
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. wage by decile
    output$wagebill_distribution <- plotly::renderPlotly({
      wagebill_distribution <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "wagebill", "wagebill_equity_decile")
      } else {
        govhr::compute_decile(
          wagebill_filtered(),
          measure_col = input$wagebill_measure,
          group_cols = group_col_to_null(input$group_filter),
          latest_measure = TRUE
        )
      }

      govhr::plot_decile(wagebill_distribution, group_col = input$group_filter)
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. wage range between 10th and 90th percentile
    output$wagebill_compression_ratio <- plotly::renderPlotly({
      wagebill_compression_ratio <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "wagebill", "wagebill_equity_compression")
      } else {
        govhr::compute_compression_ratio(
          wagebill_filtered(),
          measure_col = input$wagebill_measure,
          group_cols = group_col_to_null(input$group_filter),
          # ungrouped, each date is drawn as its own range; grouped, the
          # groups are compared on the latest date
          latest_measure = input$group_filter != "ref_date"
        )
      }

      govhr::plot_compression_ratio(
        wagebill_compression_ratio,
        group_col = input$group_filter
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}
