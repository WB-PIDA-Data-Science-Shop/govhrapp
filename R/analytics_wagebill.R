library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)

wagebill_ui <- function(id, wagebill_data) {
  bslib::layout_columns(
    bslib::card(
      bslib::card_header(
        "Wage Bill: Overview"
      ),
      bslib::card_body(
        shiny::markdown(
          readLines("inst/markdown/wagebill.md")
        )
      )
    ),
    bslib::page_sidebar(
      title = "Wagebill: Time Trend",
      sidebar = sidebar(
        title = "Wagebill Grouping",
        shiny::dateRangeInput(
          shiny::NS(id, "date_range"),
          "Date Range:",
          start = min(wagebill_data$ref_date, na.rm = TRUE),
          end = max(wagebill_data$ref_date, na.rm = TRUE),
          min = min(wagebill_data$ref_date, na.rm = TRUE),
          max = max(wagebill_data$ref_date, na.rm = TRUE)
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
      layout_columns(
        column_widths = c(12, 12),
        bslib::card(
          bslib::card_header("Wagebill Time Trend"),
          shinyWidgets::materialSwitch(
            shiny::NS(id, "toggle_growth"),
            label = "Switch to Growth Rate",
            value = FALSE
          ),
          plotly::plotlyOutput(NS(id, "wagebill_panel"))
        ),
        bslib::card(
          bslib::card_header("Wagebill Breakdown"),
          plotly::plotlyOutput(NS(id, "wagebill_cross_section"))
        )
      )
    ),
    col_widths = c(12, 12)
  )
}

wagebill_server <- function(id, wagebill_data) {
  shiny::moduleServer(id, function(input, output, session) {
    wagebill_summary <- reactive({
      if (input$wagebill_group == "all") {
        wagebill_out <- wagebill_data |>
          dplyr::filter(
            .data[["ref_date"]] >= input$date_range[1],
            .data[["ref_date"]] <= input$date_range[2]
          ) |>
          govhr::compute_fastsummary(
            cols = input$wagebill_measure,
            fns = "sum",
            groups = c("ref_date")
          )
      } else {
        wagebill_out <- wagebill_data |>
          dplyr::filter(
            .data[["ref_date"]] >= input$date_range[1],
            .data[["ref_date"]] <= input$date_range[2]
          ) |>
          govhr::compute_fastsummary(
            cols = input$wagebill_measure,
            fns = "sum",
            groups = c("ref_date", input$wagebill_group)
          )
      }

      if(input$toggle_growth) {
        validate(
          need(input$wagebill_group != "all", "Please select a group breakdown.")
        )

        wagebill_out <- wagebill_out |>
          dplyr::group_by(across(all_of(input$wagebill_group))) |>
          dplyr::arrange(.data[["ref_date"]]) |>
          dplyr::mutate(
            value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
          ) |>
          dplyr::ungroup()
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
        xlab("Time")

      if (input$wagebill_group != "all") {
        plot <- plot + 
          aes(group = .data[[input$wagebill_group]])
      }

      if(input$toggle_growth) {
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
        need(input$wagebill_group != "all", "Please select a group breakdown.") 
      )

      cross_section_data <- reactive({
        wagebill_data |>
          # only present latest year
          mutate(
            year = lubridate::year(.data[["ref_date"]])
          ) |>
          filter(
            year == max(year)
          ) |>
          govhr::compute_fastsummary(
            cols = input$wagebill_measure,
            fns = "sum",
            groups = input$wagebill_group
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
        geom_col()

      plotly::ggplotly(plot)
    }) |> 
      bindEvent(input$wagebill_group)
  })
}

wagebill_app <- function() {
  ui <- wagebill_ui("test")

  server <- function(input, output, session) {
    wagebill_server("test")
  }

  shiny::shinyApp(ui, server)
}
