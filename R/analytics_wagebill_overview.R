#' Wage Bill Overview UI
#'
#' Sidebar controls, a wage bill time trend, and totals and growth rates by
#' group.
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
#' @importFrom shinyWidgets materialSwitch
wagebill_overview_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!default_ui_controls(.data, id),
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

#' Wage Bill Overview Server
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame containing wage bill data.
#' @param cache List of pre-computed summaries from [build_analytics_cache()].
#'
#' @return A Shiny module server function.
#'
#' @import shiny
#' @importFrom dplyr left_join mutate
#' @importFrom ggplot2 aes geom_line geom_point ggplot scale_y_continuous xlab ylab
#' @importFrom govhr apply_baseline_index compute_cross_section_summary compute_fastsummary compute_growth_summary compute_trend_summary plot_bar_growth plot_bar_total plot_trend scale_plot_height
#' @importFrom lubridate year
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom purrr pluck
#' @importFrom scales percent_format
#' @importFrom dplyr filter
#' @keywords internal
wagebill_overview_server <- function(id, .data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    wagebill_filtered <- shiny::reactive({
      shiny::req(input$apply_btn)

      filter_data(
        .data,
        group_filter = input$group_filter,
        subgroup_filter = input$subgroup_filter,
        date_range = input$date_range
      )
    })

    wagebill_summary <- shiny::reactive({
      summary <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "wagebill", "wagebill_overview") |>
          dplyr::filter(
            .data[["indicator"]] == "gross_salary_lcu_sum"
          )
      } else {
        govhr::compute_trend_summary(
          wagebill_filtered(),
          group_col = input$group_filter,
          measure_col = input$wagebill_measure
        )
      }

      if (input$toggle_growth) {
        summary <- govhr::apply_baseline_index(summary, group_col = input$group_filter)
      }

      summary
    })

    # plot 1. panel
    output$wagebill_panel <- plotly::renderPlotly({
      plotly::ggplotly(
        govhr::plot_trend(
          wagebill_summary(),
          group_col = input$group_filter,
          toggle_growth = input$toggle_growth,
          y_label = "Wage Bill"
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. wage bill as a share of a macro indicator
    # NOTE: not yet wired into wagebill_overview_ui(); the sidebar has no
    # `macroindicator_measure` input, so this output is never rendered.
    wagebill_annual <- shiny::reactive({
      wagebill_filtered() |>
        govhr::compute_fastsummary(
          cols = input$wagebill_measure,
          fns = "sum",
          group_colss = c("ref_date", "country_code")
        )
    })

    output$wagebill_fiscal <- plotly::renderPlotly({
      plot <- wagebill_annual() |>
        dplyr::mutate(year = lubridate::year(.data[["ref_date"]])) |>
        dplyr::left_join(
          govhr::macro_indicators,
          by = c("country_code", "year")
        ) |>
        dplyr::mutate(
          ratio = .data[["value"]] /
            .data[[input$macroindicator_measure]] *
            100
        ) |>
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

    # plot 3. total by group
    output$wagebill_cross_section <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Please select a group."
        )
      )

      cross_section_data <- govhr::compute_cross_section_summary(
        wagebill_filtered(),
        group_col = input$group_filter,
        measure_col = input$wagebill_measure
      )

      plotly::ggplotly(
        govhr::plot_bar_total(
          cross_section_data,
          group_col = input$group_filter,
          x_label = "Wage bill"
        ),
        height = govhr::scale_plot_height(cross_section_data)
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 4. growth rate by group
    output$wagebill_change <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Please select a group."
        )
      )

      change_data <- govhr::compute_growth_summary(
        wagebill_filtered(),
        group_col = input$group_filter,
        measure_col = input$wagebill_measure
      )

      plotly::ggplotly(
        govhr::plot_bar_growth(change_data, group_col = input$group_filter),
        height = govhr::scale_plot_height(change_data)
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}
