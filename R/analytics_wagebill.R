library(shiny)
library(bslib)
library(ggplot2)

wagebill_ui <- function(id) {
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
          shiny::selectInput(
            shiny::NS(id, "wagebill_group"),
            "Group:",
            choices = list(
              "Year" = "year",
              "Establishment" = "est_id",
              "Contract type (native)" = "contract_type_native"
            )
          )
        ),
        bslib::card(
          bslib::card_header("Wagebill Analytics"),
          plotOutput(NS(id, "wagebill_plot"))
        )
      ),
      col_widths = c(12, 12)
    )
  }

wagebill_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      wagebill_data <- reactive({
        if(input$wagebill_group == "year"){
          govhr::bra_hrmis_contract |> 
            govhr::compute_fastsummary(
              cols = "gross_salary_lcu",
              fns = "sum",
              groups = c("ref_date")
            )
        }else{
          govhr::bra_hrmis_contract |> 
            govhr::compute_fastsummary(
              cols = "gross_salary_lcu",
              fns = "sum",
              groups = c("ref_date", input$wagebill_group)
            )
        }
      })

      output$wagebill_plot <- renderPlot({
        plot <- wagebill_data() |>
          ggplot(
            aes(
              x = .data[["ref_date"]],
              y = .data[["value"]]
            )
          ) +
          geom_point() +
          geom_line()

        if(input$wagebill_group != "year"){
          plot +
            facet_wrap(
              vars(.data[[input$wagebill_group]])
            )
        }else{
          plot
        }
      })
    })
  }

wagebill_app <- function(){
  ui <- wagebill_ui("test")
  
  server <- function(input, output, session){
    wagebill_server("test")
  }

  shiny::shinyApp(ui, server)
}
