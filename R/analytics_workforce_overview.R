#' Workforce Overview UI
#'
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing workforce data.
#'
#' @import shiny
#' @import bslib
#' @importFrom shinyWidgets materialSwitch
#' @importFrom plotly plotlyOutput
#'
#' @return A Shiny UI function for the workforce overview module.
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
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing workforce data.
#' @param cache A list containing cached data for performance optimization.
#'
#' @import shiny
#' @import bslib
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom shinyWidgets updatePickerInput
#'
#' @return A Shiny server function for the workforce overview module.
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

    workforce_summary <- reactive({
      # default to cache
      if(input$group_filter == "ref_date"){
        out <- cache[["workforce_trend"]]
      } else {
        out <- compute_trend_summary(
          workforce_filtered(),
          group = input$group_filter
        )
      }

      if (input$toggle_growth) {
        out <- apply_baseline_index(out, group = input$group_filter)
      }

      out
    })

    # plot 1. panel
    output$workforce_panel <- plotly::renderPlotly({
      plotly::ggplotly(
        plot_trend(
          workforce_summary(),
          group = input$group_filter,
          toggle_growth = input$toggle_growth,
          y_label = "Headcount"
        )
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. total by group
    output$workforce_cross_section <- plotly::renderPlotly({
      validate(
        need(input$group_filter != "ref_date", "Please select a group.")
      )

      cross_section_data <- compute_cross_section_summary(
        workforce_filtered(),
        group = input$group_filter
      )

      n_groups <- nrow(cross_section_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_total(
          cross_section_data,
          group = input$group_filter,
          x_label = "Headcount"
        ),
        height = plot_height
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth rate by group
    output$workforce_growth <- plotly::renderPlotly({
      validate(
        need(input$group_filter != "ref_date", "Please select a group.")
      )

      change_data <- compute_growth_summary(
        workforce_filtered(),
        group = input$group_filter
      )

      n_groups <- nrow(change_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_growth(change_data, group = input$group_filter),
        height = plot_height
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}
