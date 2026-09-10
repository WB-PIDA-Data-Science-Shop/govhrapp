#' Workforce Transition UI
#'
#' Sidebar controls, a transition trend plot and an interactive transition
#' network.
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame containing wage bill data.
#'
#' @return A Shiny UI definition.
#'
#' @importFrom bslib card card_header layout_sidebar popover sidebar
#' @importFrom ggiraph girafeOutput
#' @importFrom plotly plotlyOutput
#' @importFrom shiny NS actionButton selectInput
workforce_transition_ui <- function(id, .data) {
  choices <- identify_group_choices(.data)[c("Personnel", "Contract")]

  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      date_ui(id, .data),
      group_filter_ui(
        id, .data, selected = "contract_type", group_choices = choices
      ),
      subgroup_filter_ui(id),
      shiny::selectInput(
        shiny::NS(id, "id_col"),
        "Identifier",
        choices = c("Personnel" = "personnel_id", "Contract" = "contract_id"),
        selected = "personnel_id"
      ),
      shiny::actionButton(
        shiny::NS(id, "apply_btn"),
        "Apply selection",
        icon = shiny::icon("play")
      )
    ),

    # plot 1. transitions over time
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Transitions over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "The number of internal transitions over time. This is computed as the number of internal transitions over the entire time period, considering as a transition a movement of personnel across groups between each reference date.",
          title = "Transitions over time",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "transition_trend_plot"))
    ),

    # plot 2. transition network
    bslib::card(
      height = "600px",
      full_screen = TRUE,
      bslib::card_header(
        "Transition Network",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "The number of internal transitions over time. This is computed as the number of internal transitions over the entire time period, considering as a transition a movement of personnel across groups between each reference date.",
          title = "Transition Network",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      ggiraph::girafeOutput(shiny::NS(id, "transition_network_plot"))
    )
  )
}

#' Workforce Transition Server
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame containing wage bill data.
#' @param cache List of pre-computed summaries from [build_analytics_cache()].
#'
#' @return A Shiny module server function.
#'
#' @importFrom ggiraph renderGirafe
#' @importFrom govhr detect_career_transition fastcount plot_transition_network plot_trend
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom purrr pluck
#' @importFrom shiny bindEvent moduleServer reactive req
#'
#' @export
workforce_transition_server <- function(id, .data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    workforce_filtered <- shiny::reactive({
      shiny::req(input$apply_btn)

      filter_data(
        .data,
        group_filter = input$group_filter,
        subgroup_filter = input$subgroup_filter,
        date_range = input$date_range
      )
    })

    # both plots read the same transitions, so detect them once per apply
    transition_data <- shiny::reactive({
      if (input$apply_btn == 0) {
        purrr::pluck(cache, "workforce", "workforce_transition")
      } else {
        govhr::detect_career_transition(
          workforce_filtered(),
          id_col = input$id_col,
          group_cols = input$group_filter
        )
      }
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 1. transitions over time
    output$transition_trend_plot <- plotly::renderPlotly({
      plotly::ggplotly(
        transition_data() |>
          govhr::fastcount(ref_date, name = "transition") |>
          govhr::plot_trend(
            group_col = "ref_date",
            y_col = "transition",
            y_label = "Number of Transitions"
          )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. transition network
    output$transition_network_plot <- ggiraph::renderGirafe({
      govhr::plot_transition_network(transition_data())
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}

#' Launch the Workforce Transition Module Standalone
#'
#' @param .data Data frame containing wage bill data.
#' @param cache List of pre-computed summaries from [build_analytics_cache()].
#'
#' @return A Shiny app object.
#'
#' @importFrom shiny shinyApp
#' @keywords internal
workforce_transition_app <- function(.data, cache) {
  shiny::shinyApp(
    ui = workforce_transition_ui("transition", .data),
    server = function(input, output, session) {
      workforce_transition_server("transition", .data, cache)
    }
  )
}
