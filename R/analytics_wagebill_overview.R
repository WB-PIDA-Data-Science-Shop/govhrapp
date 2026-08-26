
#' Function to create the UI for the wagebill overview module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#'
#' @import shiny
#' @import bslib
#' @importFrom shinyWidgets materialSwitch
#' @importFrom plotly plotlyOutput
#' @importFrom bsicons bs_icon
#' 
#' @return A Shiny module UI function for the wagebill overview module.
wagebill_overview_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!ui_filter_controls(.data, id),
      # conditionally allow user to select wagebill measure if available
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
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Wage Bill Time Trends",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Wage bill trends over time. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_panel"),
        height = "350px"
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        fillable = FALSE,
        bslib::card_header(
          "Total by group",
          bslib::popover(
            bsicons::bs_icon("info-circle-fill"),
            "Total amount, by group. Total refers to the latest available year in the selected time frame.",
            placement = "left"
          ),
          class = "d-flex justify-content-between"
        ),
        plotly::plotlyOutput(shiny::NS(id, "wagebill_cross_section")),
        min_height = "450px"
      ),
      bslib::card(
        full_screen = TRUE,
        fillable = FALSE,
        bslib::card_header(
          "Growth rate by group",
          bslib::popover(
            bsicons::bs_icon("info-circle-fill"),
            "Growth rate with respect to first reference date, by group.",
            placement = "left"
          ),
          class = "d-flex justify-content-between"
        ),
        plotly::plotlyOutput(shiny::NS(id, "wagebill_change")),
        min_height = "450px"
      )
    )
  )
}

#' Function to create the server logic for the wagebill overview module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#' @param cache A list containing pre-computed trend summaries for workforce and wagebill data.
#' 
#' @import shiny
#' @importFrom plotly renderPlotly
#' 
#' @return A Shiny module server function for the wagebill overview module.
wagebill_overview_server <- function(id, .data, cache) {
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

    wagebill_summary <- shiny::reactive({
      # default to cache
      if(input$group_filter == "ref_date") {
        out <- cache
      } else {
        out <- compute_trend_summary(
          wagebill_filtered(),
          group = input$group_filter,
          measure_col = input$wagebill_measure
        )
      }

      if (input$toggle_growth) {
        out <- apply_baseline_index(out, group = input$group_filter)
      }

      out
    })

    wagebill_annual <- shiny::reactive({
      wagebill_filtered() |>
        govhr::compute_fastsummary(
          cols = input$wagebill_measure,
          fns = "sum",
          groups = c("ref_date", "country_code")
        )
    })

    # plot 1. panel
    output$wagebill_panel <- plotly::renderPlotly({
      plotly::ggplotly(
        plot_trend(
          wagebill_summary(),
          group = input$group_filter,
          toggle_growth = input$toggle_growth,
          y_label = "Wage Bill"
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. macro panel
    output$wagebill_fiscal <- renderPlotly({
      wagebill_fiscal <- wagebill_annual() |>
        mutate(
          year = lubridate::year(ref_date)
        ) |>
        left_join(
          govhr::macro_indicators,
          by = c("country_code", "year")
        ) |>
        mutate(
          ratio = .data[["value"]] / .data[[input$macroindicator_measure]] * 100
        )

      plot <- wagebill_fiscal |>
        ggplot2::ggplot(
          ggplot2::aes(x = .data[["ref_date"]], y = .data[["ratio"]])
        ) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::xlab("Time") +
        ggplot2::ylab("Ratio") +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(scale = 1)
        )

      plotly::ggplotly(plot)
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. total by group
    output$wagebill_cross_section <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Please select a group."
        )
      )

      cross_section_data <- compute_cross_section_summary(
        wagebill_filtered(),
        group = input$group_filter,
        measure_col = input$wagebill_measure
      )

      n_groups <- nrow(cross_section_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_total(
          cross_section_data,
          group = input$group_filter,
          x_label = "Wage bill"
        ),
        height = plot_height
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth rate by group
    output$wagebill_change <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Please select a group."
        )
      )

      change_data <- compute_growth_summary(
        wagebill_filtered(),
        group = input$group_filter,
        measure_col = input$wagebill_measure
      )

      n_groups <- nrow(change_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_growth(change_data, group = input$group_filter),
        height = plot_height
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}
