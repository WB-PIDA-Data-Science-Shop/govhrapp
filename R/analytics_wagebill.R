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
            "Contract type (native)" = "contract_type_native"
          )
        )
      ),
      layout_columns(
        column_widths = c(12, 12),
        bslib::card(
          bslib::card_header("Wagebill Time Trend"),
          plotlyOutput(NS(id, "wagebill_panel"))
        ),
        bslib::card(
          bslib::card_header("Wagebill Breakdown"),
          plotlyOutput(NS(id, "wagebill_cross_section"))
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
        wagebill_data |>
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
        wagebill_data |>
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
    })

    # plot 1. panel
    output$wagebill_panel <- renderPlotly({
      plot <- wagebill_summary() |>
        ggplot(
          aes(
            x = .data[["ref_date"]],
            y = .data[["value"]]
          )
        ) +
        geom_point() +
        geom_line()

    if (input$wagebill_group != "all") {
      plot <- plot + 
        aes(group = .data[[input$wagebill_group]], 
            color = .data[[input$wagebill_group]])
    }

      plotly::ggplotly(plot)
    })

    # plot 2. cross-section
    output$wagebill_cross_section <- renderPlotly({
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
            y = reorder(
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
