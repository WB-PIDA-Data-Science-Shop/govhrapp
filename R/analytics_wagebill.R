#' Wage Bill UI Module
#'
#' Creates the UI for the Wage Bill analytics section of the dashboard,
#' displaying overview information and time trend visualizations.
#'
#' @param id Character string. The module namespace ID.
#'
#' @return A Shiny UI element containing wage bill analytics cards.
#'
#' @importFrom shiny NS selectInput markdown plotOutput
#' @importFrom bslib layout_columns card card_header card_body page_sidebar sidebar
#'
#' @keywords internal
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

#' Wage Bill Server Module
#'
#' Server logic for the Wage Bill section, processing and visualizing
#' wage bill time trends grouped by various dimensions.
#'
#' @param id Character string. The module namespace ID.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @importFrom shiny moduleServer reactive renderPlot
#' @importFrom ggplot2 ggplot aes geom_point geom_line facet_wrap vars
#' @importFrom rlang .data
#'
#' @keywords internal
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

#' Standalone Wage Bill Application
#'
#' Launches a standalone Shiny app for testing the wage bill module.
#'
#' @return A Shiny app object.
#'
#' @importFrom shiny shinyApp
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' wagebill_app()
#' }
wagebill_app <- function(){
  ui <- wagebill_ui("test")
  
  server <- function(input, output, session){
    wagebill_server("test")
  }

  shiny::shinyApp(ui, server)
}
