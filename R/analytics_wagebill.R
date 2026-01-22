library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)

wagebill_data <- govhr::bra_hrmis_contract |>
  dplyr::filter(
    lubridate::year(.data[["ref_date"]]) <= 2017
  )

wagebill_ui <- function(id, wagebill_data) {
  bslib::layout_columns(
    fillable = FALSE,
    bslib::card(
      bslib::card_header(
        "Wage Bill: Overview"
      ),
      bslib::card_body(
        shiny::markdown(
          readLines("inst/markdown/wagebill.md")
        ),
        height = "350px"
      )
    ),
    bslib::layout_sidebar(
      fillable = FALSE,
      title = "Wagebill: Time Trend",
      sidebar = sidebar(
        title = "Controls",
        shinyWidgets::numericRangeInput(
          shiny::NS(id, "date_range"),
          "Time frame:",
          value = c(
            min(lubridate::year(wagebill_data$ref_date), na.rm = TRUE),
            max(lubridate::year(wagebill_data$ref_date), na.rm = TRUE)
          ),
          min = min(lubridate::year(wagebill_data$ref_date), na.rm = TRUE),
          max = max(lubridate::year(wagebill_data$ref_date), na.rm = TRUE)
        ),
        shiny::selectInput(
          shiny::NS(id, "wagebill_measure"),
          "Type of Wage:",
          choices = list(
            "Base Salary" = "base_salary_lcu",
            "Gross Salary" = "gross_salary_lcu",
            "Net Salary" = "net_salary_lcu"
          )
        ),
        shiny::selectInput(
          shiny::NS(id, "wagebill_group"),
          "Group:",
          choices = list(
            "All" = "all",
            "Establishment" = "est_id",
            "Contract type (native)" = "contract_type_native",
            "Paygrade" = "paygrade",
            "Occupation" = "occupation_native"
          )
        )
      ),
      bslib::card(
        bslib::card_header("Time trends"),
        shinyWidgets::materialSwitch(
          shiny::NS(id, "toggle_growth"),
          label = "Switch to baseline index",
          value = FALSE
        ),
        plotly::plotlyOutput(NS(id, "wagebill_panel")),
        height = "350px"
      ),
      layout_columns(
        bslib::card(
          bslib::card_header("Total by group"),
          plotly::plotlyOutput(NS(id, "wagebill_cross_section")),
          height = "450px"
        ),
        bslib::card(
          bslib::card_header("Growth rate by group"),
          plotly::plotlyOutput(NS(id, "wagebill_change")),
          height = "450px"
        )
      ),
      bslib::card(
        bslib::card_header("Equity"),
        plotly::plotlyOutput(NS(id, "wagebill_dispersion")),
        height = "450px"
      )
    ),
    col_widths = c(12, 12)
  )
}

wagebill_server <- function(id, wagebill_data) {
  shiny::moduleServer(id, function(input, output, session) {
    wagebill_filtered_date <- reactive({
      wagebill_data |>
        mutate(
          year = lubridate::year(.data[["ref_date"]])
        ) |>
        dplyr::filter(
          .data[["year"]] >= input$date_range[1],
          .data[["year"]] <= input$date_range[2]
        )
    })

    wagebill_summary <- reactive({
      if (input$wagebill_group == "all") {
        wagebill_out <- wagebill_filtered_date() |>
          govhr::compute_fastsummary(
            cols = input$wagebill_measure,
            fns = "sum",
            groups = c("ref_date")
          )
      } else {
        wagebill_out <- wagebill_filtered_date() |>
          govhr::compute_fastsummary(
            cols = input$wagebill_measure,
            fns = "sum",
            groups = c("ref_date", input$wagebill_group)
          )
      }

      # if growth rate toggle is on
      if (input$toggle_growth) {
        if (input$wagebill_group == "all") {
          wagebill_out <- wagebill_out |>
            dplyr::arrange(.data[["ref_date"]]) |>
            dplyr::mutate(
              value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
            )
        } else {
          wagebill_out <- wagebill_out |>
            dplyr::group_by(across(all_of(input$wagebill_group))) |>
            dplyr::arrange(.data[["ref_date"]]) |>
            dplyr::mutate(
              value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
            ) |>
            dplyr::ungroup()
        }
      }

      wagebill_out
    })

    # plot 1. panel
    output$wagebill_panel <- plotly::renderPlotly({
      plot <- wagebill_summary() |>
        ggplot(
          aes(
            x = .data[["ref_date"]],
            y = .data[["value"]]
          )
        ) +
        geom_point() +
        geom_line() +
        scale_y_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ) +
        xlab("Time")

      if (input$wagebill_group != "all") {
        plot <- plot +
          aes(group = .data[[input$wagebill_group]])
      }

      if (input$toggle_growth) {
        plot <- plot +
          ylab("Growth Rate (Base = 100%)") +
          geom_hline(
            yintercept = 100,
            linetype = "dashed",
            color = "red3"
          )
      } else {
        plot <- plot +
          ylab("Wage Bill (LCU)")
      }

      plotly::ggplotly(plot)
    })

    # plot 2. cross-section
    output$wagebill_cross_section <- plotly::renderPlotly({
      validate(
        need(input$wagebill_group != "all", "Please select a group.")
      )

      cross_section_data <- reactive({
        wagebill_filtered_date() |>
          # only present latest year
          filter(
            year == max(year)
          ) |>
          govhr::compute_fastsummary(
            cols = input$wagebill_measure,
            fns = "sum",
            groups = input$wagebill_group
          ) |>
          # drop missing values and groups
          filter(
            !is.na(.data[["value"]]) &
              !is.na(.data[[input$wagebill_group]])
          )
      })

      plot <- cross_section_data() |>
        ggplot(
          aes(
            x = .data[["value"]],
            y = stats::reorder(
              .data[[input$wagebill_group]],
              .data[["value"]]
            )
          )
        ) +
        geom_col() +
        scale_x_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ) +
        labs(
          x = "Wage bill",
          y = ""
        )

      plotly::ggplotly(plot)
    }) |>
      bindEvent(input$wagebill_group)

    # plot 3. comparative change
    output$wagebill_change <- plotly::renderPlotly({
      validate(
        need(input$wagebill_group != "all", "Please select a group.")
      )

      change_data <- reactive({
        wagebill_filtered_date() |>
          # only present latest date
          dplyr::filter(
            year %in% c(max(year), max(year) - 1)
          ) |>
          govhr::compute_fastsummary(
            cols = input$wagebill_measure,
            fns = "sum",
            groups = c("ref_date", input$wagebill_group)
          ) |>
          dplyr::group_by(
            across(all_of(input$wagebill_group))
          ) |>
          govhr::complete_dates(
            id_col = input$wagebill_group,
            start_date = max(wagebill_data$ref_date) - lubridate::years(1),
            end_date = max(wagebill_data$ref_date)
          ) |>
          dplyr::mutate(
            growth_rate = round(
              .data[["value"]] / dplyr::lag(.data[["value"]]) - 1,
              3
            ) *
              100
          ) |>
          filter(
            # filter only latest value
            .data[["ref_date"]] == max(wagebill_data$ref_date) &
              # drop missing values and groups
              !is.na(.data[["growth_rate"]]) &
              !is.na(.data[[input$wagebill_group]])
          )
      })

      plot <- change_data() |>
        ggplot(
          aes(
            x = .data[["growth_rate"]],
            y = stats::reorder(
              .data[[input$wagebill_group]],
              .data[["growth_rate"]]
            )
          )
        ) +
        geom_col() +
        geom_vline(
          xintercept = 0,
          linetype = "dashed",
          color = "orangered2"
        ) +
        scale_x_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ) +
        labs(
          x = "Growth rate",
          y = ""
        )

      plotly::ggplotly(plot)
    }) |>
      bindEvent(input$wagebill_group)

    # plot 4. pay dispersion across groups
    output$wagebill_dispersion <- plotly::renderPlotly({
      dispersion_data <- reactive({
        wagebill_deflated <- wagebill_filtered_date() |>
          govhr::convert_constant_ppp(
            cols = input$wagebill_measure,
            macro_indicators = govhr::macro_indicators
          )
      })

      dispersion_data() |>
        plot_segment(
          col = input$wagebill_measure,
          group = input$wagebill_group
        ) |>
        plotly::ggplotly()
    })
  })
}

#' Run the Wage Bill Shiny Application
#'
#' Launches an interactive Shiny application for analyzing wage bill data,
#' including time trends, cross-sectional comparisons, and growth rate analysis.
#'
#' @param wagebill_data A data frame containing wage bill information with the
#'   following required columns:
#'   \itemize{
#'     \item \code{ref_date}: Reference date (Date class)
#'     \item \code{base_salary_lcu}: Base salary in local currency units
#'     \item \code{gross_salary_lcu}: Gross salary in local currency units
#'     \item \code{net_salary_lcu}: Net salary in local currency units
#'     \item \code{est_id}: Establishment identifier
#'     \item \code{contract_type_native}: Contract type
#'     \item \code{paygrade}: Paygrade classification
#'     \item \code{occupation_native}: Occupation classification
#'   }
#'   Defaults to filtered Brazilian HRMIS contract data (years <= 2017).
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @return A Shiny app object.
#'
#' @details
#' The application provides four main analytical views:
#' \itemize{
#'   \item Time trend analysis with optional baseline indexing
#'   \item Cross-sectional wage bill totals by group
#'   \item Year-over-year growth rates by group
#'   \item Interactive filtering by time period, wage type, and grouping variable
#' }
#'
#' @examples
#' \dontrun{
#' # Run with default data
#' run_wagebillapp()
#'
#' # Run with custom data
#' my_data <- govhr::bra_hrmis_contract |>
#'   dplyr::filter(lubridate::year(ref_date) >= 2015)
#' run_wagebillapp(wagebill_data = my_data)
#' }
#'
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @export
run_wagebillapp <- function(
  wagebill_data = govhr::bra_hrmis_contract |>
    dplyr::filter(lubridate::year(.data[["ref_date"]]) <= 2017),
  ...
) {
  ui <- wagebill_ui("test", wagebill_data)

  server <- function(input, output, session) {
    wagebill_server("test", wagebill_data)
  }

  shiny::shinyApp(ui, server)
}
