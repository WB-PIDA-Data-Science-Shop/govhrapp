#' Function to create the UI for the wagebill equity module.
#'
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#'
#' @import bslib
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom bsicons bs_icon
#'
#' @return A Shiny module UI function for the wagebill equity module.
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

#' Function to create the server logic for the wagebill equity module.
#'
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#'
#' @import shiny
#' @importFrom plotly renderPlotly
#' @importFrom dplyr filter
#' @importFrom govhr compute_compression_ratio
#'
#' @return A Shiny module server function for the wagebill equity module.
wagebill_equity_server <- function(id, .data) {
  shiny::moduleServer(id, function(input, output, session) {
    # choice of cols
    wagebill_group_choices <- identify_group_choices(.data)

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
      wagebill_density <- wagebill_filtered() |>
        dplyr::filter(.data[["ref_date"]] == max(.data[["ref_date"]])) |>
        compute_percentile(
          group_col = input$group_filter,
          binwidth = 100,
          measure_col = input$wagebill_measure
        )

      plotly::ggplotly(
        plot_histogram(
          wagebill_density,
          plot_type = input$plot_type,
          group_col = input$group_filter
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. wage by decile
    output$wagebill_distribution <- plotly::renderPlotly({
      # filter latest ref_date
      latest_ref_date <- max(wagebill_filtered()[["ref_date"]])

      wagebill_filtered_latest <- wagebill_filtered() |>
        dplyr::filter(.data[["ref_date"]] == latest_ref_date)

      wagebill_distribution <- compute_decile(
        wagebill_filtered_latest,
        group_cols = input$group_filter,
        measure_col = input$wagebill_measure,
        latest_measure = TRUE
      )

      n_groups <- nrow(wagebill_distribution)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_decile(
          wagebill_distribution,
          group_cols = input$group_filter
        ),
        height = plot_height
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. wage range between 10th and 90th percentile
    output$wagebill_compression_ratio <- plotly::renderPlotly({
      wagebill_compression_ratio <- govhr::compute_compression_ratio(
        wagebill_filtered(),
        group_cols = input$group_filter,
        measure_col = input$wagebill_measure,
        latest_measure = TRUE
      )

      n_groups <- nrow(wagebill_compression_ratio)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_compression_ratio(
          wagebill_compression_ratio,
          group_cols = input$group_filter
        ),
        height = plot_height
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}
