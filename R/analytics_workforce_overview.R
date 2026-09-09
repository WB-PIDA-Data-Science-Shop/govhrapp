#' Workforce Overview UI
#'
#' Sidebar controls, a headcount time trend, and totals and growth rates by
#' group.
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame containing workforce data.
#'
#' @return A Shiny UI definition.
#'
#' @import bslib
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom shinyWidgets materialSwitch
workforce_overview_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!default_ui_controls(.data, id),
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
        "Headcount",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Headcount trends over time. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "workforce_panel"),
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
            "Headcount total, by group. Total refers to the latest available year in the selected time frame.",
            placement = "left"
          ),
          class = "d-flex justify-content-between"
        ),
        plotly::plotlyOutput(shiny::NS(id, "workforce_cross_section")),
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
        plotly::plotlyOutput(shiny::NS(id, "workforce_growth")),
        min_height = "450px"
      )
    )
  )
}

#' Workforce Overview Server
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame containing workforce data.
#' @param cache List of pre-computed summaries from [build_analytics_cache()].
#'
#' @return A Shiny module server function.
#'
#' @import shiny
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom purrr pluck
#' @keywords internal
workforce_overview_server <- function(id, .data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    workforce_filtered <- shiny::reactive({
      filter_data(
        .data,
        group_filter = input$group_filter,
        subgroup_filter = input$subgroup_filter,
        date_range = input$date_range
      )
    })

    workforce_summary <- shiny::reactive({
      summary <- if (input$apply_btn == 0) {
        purrr::pluck(cache, "workforce", "workforce_overview")
      } else {
        compute_trend_summary(
          workforce_filtered(),
          group_col = input$group_filter
        )
      }

      if (input$toggle_growth) {
        summary <- govhr::apply_baseline_index(summary, group_col = input$group_filter)
      }

      summary
    })

    # plot 1. panel
    output$workforce_panel <- plotly::renderPlotly({
      plotly::ggplotly(
        govhr::plot_trend(
          workforce_summary(),
          group_col = input$group_filter,
          toggle_growth = input$toggle_growth,
          y_label = "Headcount"
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. total by group
    output$workforce_cross_section <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$group_filter != "ref_date", "Please select a group.")
      )

      cross_section_data <- compute_cross_section_summary(
        workforce_filtered(),
        group_col = input$group_filter
      )

      plotly::ggplotly(
        govhr::plot_bar_total(
          cross_section_data,
          group_col = input$group_filter,
          x_label = "Headcount"
        ),
        height = scale_plot_height(cross_section_data)
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth rate by group
    output$workforce_growth <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$group_filter != "ref_date", "Please select a group.")
      )

      change_data <- compute_growth_summary(
        workforce_filtered(),
        group_col = input$group_filter
      )

      plotly::ggplotly(
        govhr::plot_bar_growth(change_data, group_col = input$group_filter),
        height = scale_plot_height(change_data)
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}
