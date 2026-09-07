#' Wage Bill Equity UI
#'
#' Sidebar controls and plots for wage distribution, deciles and compression.
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
wagebill_equity_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!default_ui_controls(.data, id),
      shiny::selectInput(
        shiny::NS(id, "wagebill_measure"),
        "Type of Wage:",
        choices = identify_wagebill_choices(.data)
      ),
      shiny::radioButtons(
        inputId = shiny::NS(id, "plot_type"),
        label = "Type of wage distribution",
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
        "Wage Distribution",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Wage density distribution. Choosing a group will add new trend lines, by group.",
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
        "Wage by Decile",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Wage distribution by decile. Choosing a group will add new trend lines, by group.",
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
        "Wage Compression Ratio (10th to 90th Percentile)",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Wage compression ratio between the 10th and 90th percentile. Choosing a group will add new trend lines, by group.",
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

#' Wage Bill Equity Server
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame containing wage bill data.
#' @param cache List of pre-computed summaries from [build_analytics_cache()].
#'
#' @return A Shiny module server function.
#'
#' @import shiny
#' @importFrom govhr compute_compression_ratio
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
      )
    })

    # plot 1. wage density
    output$wagebill_density <- plotly::renderPlotly({
      wagebill_density <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "wagebill", "wagebill_equity_percentile")
      } else {
        compute_percentile(
          wagebill_filtered(),
          group_col = input$group_filter,
          binwidth = 100,
          measure_col = input$wagebill_measure
        )
      }

      plot_histogram(
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
        compute_decile(
          wagebill_filtered(),
          group_cols = input$group_filter,
          measure_col = input$wagebill_measure,
          latest_measure = TRUE
        )
      }

      plot_decile(wagebill_distribution, group_col = input$group_filter)
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. wage range between 10th and 90th percentile
    output$wagebill_compression_ratio <- plotly::renderPlotly({
      wagebill_compression_ratio <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "wagebill", "wagebill_equity_compression")
      } else {
        compute_compression_ratio(
          wagebill_filtered(),
          group_cols = input$group_filter,
          measure_col = input$wagebill_measure
        )
      }

      plot_compression_ratio(
        wagebill_compression_ratio,
        group_col = input$group_filter
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}
