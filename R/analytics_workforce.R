library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)

workforce_data <- govhr::bra_hrmis_contract |>
  dplyr::filter(lubridate::year(.data[["ref_date"]]) <= 2017) |> 
  dplyr::left_join(
    govhr::bra_hrmis_personnel |>
      distinct(.data[["ref_date"]], .data[["personnel_id"]], .keep_all = TRUE) |> 
      select(.data[["ref_date"]], .data[["personnel_id"]], .data[["gender"]], .data[["educat7"]], .data[["status"]]),
    by = c("ref_date", "personnel_id")
  )

workforce_ui <- function(id, workforce_data) {
  bslib::layout_columns(
    fillable = FALSE,
    bslib::card(
      bslib::card_header(
        "Workforce: Overview"
      ),
      bslib::card_body(
        shiny::markdown(
          readLines("inst/markdown/workforce.md")
        ),
        height = "300px"
      )
    ),
    bslib::accordion(
      bslib::accordion_panel(
        title = "Guidance Questions",
        icon = shiny::icon("question-circle"),
        shiny::markdown(
          readLines("inst/markdown/workforce_questions.md")
        )
      ),
      open = FALSE
    ),
    bslib::layout_sidebar(
      fillable = FALSE,
      title = "Workforce: Time Trend",
      sidebar = bslib::sidebar(
        title = "Controls",
        width = "300px",
        shinyWidgets::numericRangeInput(
          shiny::NS(id, "date_range"),
          "Time frame:",
          value = c(
            min(lubridate::year(workforce_data$ref_date), na.rm = TRUE),
            max(lubridate::year(workforce_data$ref_date), na.rm = TRUE)
          ),
          min = min(lubridate::year(workforce_data$ref_date), na.rm = TRUE),
          max = max(lubridate::year(workforce_data$ref_date), na.rm = TRUE)
        ),
        shiny::selectInput(
          shiny::NS(id, "workforce_group"),
          "Group:",
          choices = list(
            "All" = "ref_date",
            "Establishment" = "est_id",
            "Contract" = c(
              "Contract type (native)" = "contract_type_native",
              "Paygrade" = "paygrade",
              "Occupation" = "occupation_native"
            ),
            "Personnel" = c(
              "Gender" = "gender",
              "Education" = "educat7",
              "Employment Status" = "status"
            )
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
        plotly::plotlyOutput(NS(id, "workforce_panel")),
        height = "350px"
      ),
      layout_columns(
        bslib::card(
          bslib::card_header("Total by group"),
          plotly::plotlyOutput(NS(id, "workforce_cross_section")),
          height = "450px"
        ),
        bslib::card(
          bslib::card_header("Growth dynamics by group"),
          plotly::plotlyOutput(NS(id, "workforce_change")),
          height = "450px"
        )
      )
    ),
    col_widths = c(12, 12)
  )
}

workforce_server <- function(id, workforce_data) {
  shiny::moduleServer(id, function(input, output, session) {
    workforce_filtered_date <- reactive({
      workforce_data |>
        mutate(
          year = lubridate::year(.data[["ref_date"]])
        ) |>
        dplyr::filter(
          .data[["year"]] >= input$date_range[1],
          .data[["year"]] <= input$date_range[2]
        )
    })

    workforce_summary <- reactive({
      if (input$workforce_group == "ref_date") {
        workforce_out <- workforce_filtered_date() |>
          govhr::fastcount(.data[["ref_date"]], name = "value")
      } else {
        workforce_out <- workforce_filtered_date() |>
          govhr::fastcount(.data[["ref_date"]], .data[[input$workforce_group]], name = "value")
      }

      # if growth rate toggle is on
      if (input$toggle_growth) {
        if (input$workforce_group == "ref_date") {
          workforce_out <- workforce_out |>
            dplyr::arrange(.data[["ref_date"]]) |>
            dplyr::mutate(
              value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
            )
        } else {
          workforce_out <- workforce_out |>
            dplyr::group_by(across(all_of(input$workforce_group))) |>
            dplyr::arrange(.data[["ref_date"]]) |>
            dplyr::mutate(
              value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
            ) |>
            dplyr::ungroup()
        }
      }

      workforce_out
    })

    # plot 1. panel
    output$workforce_panel <- plotly::renderPlotly({
      plot <- workforce_summary() |>
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

      if (input$workforce_group != "ref_date") {
        plot <- plot +
          aes(group = .data[[input$workforce_group]])
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
          ylab("Headcount")
      }

      plotly::ggplotly(plot)
    })

    # plot 2. total by group
    output$workforce_cross_section <- plotly::renderPlotly({
      validate(
        need(input$workforce_group != "ref_date", "Please select a group.")
      )

      cross_section_data <- reactive({
        workforce_filtered_date() |>
          # only present latest year
          filter(
            year == max(year)
          ) |>
          govhr::fastcount(.data[[input$workforce_group]], name = "value") |>
          # drop missing values and groups
          filter(
            !is.na(.data[["value"]]) &
              !is.na(.data[[input$workforce_group]])
          )
      })      

      plot <- cross_section_data() |>
        ggplot(
          aes(
            x = .data[["value"]],
            y = stats::reorder(
              .data[[input$workforce_group]],
              .data[["value"]]
            )
          )
        ) +
        geom_col() +
        scale_x_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ) +
        labs(
          x = "Headcount",
          y = ""
        )

      plotly::ggplotly(plot)
    }) |>
      bindEvent(input$workforce_group, input$date_range)

    # plot 3. growth rate by group
    output$workforce_change <- plotly::renderPlotly({
      validate(
        need(input$workforce_group != "ref_date", "Please select a group.")
      )

      change_data <- reactive({
        workforce_annual <- workforce_filtered_date() |>
          # only present latest year and previous one
          dplyr::filter(
            year %in% c(max(year), max(year) - 1)
          ) |>
          govhr::fastcount(.data[["ref_date"]], .data[[input$workforce_group]], name = "value")

        workforce_annual |>
          dplyr::group_by(
            across(all_of(input$workforce_group))
          ) |>
          govhr::complete_dates(
            id_col = input$workforce_group,
            start_date = max(workforce_data$ref_date) - lubridate::years(1),
            end_date = max(workforce_data$ref_date)
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
            .data[["ref_date"]] == max(.data[["ref_date"]]) &
              # drop missing values and groups
              !is.na(.data[["growth_rate"]]) &
              !is.na(.data[[input$workforce_group]])
          )
      })

      plot <- change_data() |>
        ggplot(
          aes(
            x = .data[["growth_rate"]],
            y = stats::reorder(
              .data[[input$workforce_group]],
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
      bindEvent(input$workforce_group, input$date_range)
  })
}

#' Run the Workforce Shiny Application
#'
#' Launches an interactive Shiny application for analyzing workforce headcount data,
#' including time trends, cross-sectional comparisons, and growth rate analysis.
#'
#' @param workforce_data A data frame containing workforce information with the
#'   following required columns:
#'   \itemize{
#'     \item \code{ref_date}: Reference date (Date class)
#'     \item \code{personnel_id}: Personnel identifier
#'     \item \code{est_id}: Establishment identifier
#'     \item \code{contract_type_native}: Contract type
#'     \item \code{paygrade}: Paygrade classification
#'     \item \code{occupation_native}: Occupation classification
#'     \item \code{gender}: Gender
#'     \item \code{educat7}: Education level
#'     \item \code{status}: Employment status
#'   }
#'   Defaults to filtered Brazilian HRMIS contract data (years <= 2017).
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @return A Shiny app object.
#'
#' @details
#' The application provides three main analytical views:
#' \itemize{
#'   \item Time trend analysis with optional baseline indexing
#'   \item Cross-sectional headcount totals by group
#'   \item Year-over-year growth rates by group
#'   \item Interactive filtering by time period and grouping variable
#' }
#'
#' @examples
#' \dontrun{
#' # Run with default data
#' run_workforceapp()
#'
#' # Run with custom data
#' my_data <- govhr::bra_hrmis_contract |>
#'   dplyr::filter(lubridate::year(ref_date) >= 2015)
#' run_workforceapp(workforce_data = my_data)
#' }
#'
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @export
run_workforceapp <- function(
  workforce_data,
  ...
) {
  ui <- workforce_ui("test", workforce_data)

  server <- function(input, output, session) {
    workforce_server("test", workforce_data)
  }

  shiny::shinyApp(ui, server)
}
