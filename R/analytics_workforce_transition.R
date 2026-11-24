#' Workforce transition panel UI
#'
#' Sidebar controls, a trend of transitions between groups, and an interactive
#' network of the transitions.
#'
#' @param id Character. Module namespace ID.
#' @param .data Contract data, as `wagebill_data` in
#'   [run_govhrapp_analytics()]. Transitions are counted between personnel or
#'   contract groups, such as `contract_type`, which only contract data holds.
#'
#' @returns A Shiny UI definition.
#'
#' @importFrom bslib card card_header layout_sidebar popover sidebar
#' @importFrom ggiraph girafeOutput
#' @importFrom plotly plotlyOutput
#' @importFrom shiny NS actionButton selectInput
#' @keywords internal
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
        "Count moves of",
        choices = c("Personnel" = "personnel_id", "Contracts" = "contract_id"),
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
          "Number of moves from one subgroup to another since the previous reference date, counting personnel or contracts as chosen in the sidebar.",
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
        "Transition network",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Moves between subgroups, added up over the selected date range.",
          title = "Transition network",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      ggiraph::girafeOutput(shiny::NS(id, "transition_network_plot"))
    )
  )
}

#' Workforce transition panel server
#'
#' Renders the transition trend and network, from the cache until the user
#' applies a selection.
#'
#' @inheritParams workforce_transition_ui
#' @param cache List of precomputed summaries from [build_analytics_cache()].
#'
#' @returns Called for its side effect of rendering the module's outputs.
#'
#' @importFrom dplyr collect
#' @importFrom ggiraph renderGirafe
#' @importFrom govhr compute_transition fastcount plot_transition plot_trend
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom purrr pluck
#' @importFrom shiny bindEvent moduleServer need reactive req validate
#'
#' @keywords internal
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

    # both plots read the same transitions, so detect them once per apply. they
    # are collected so a lazy table's query is not rerun for each plot, and so
    # nrow() can tell when the selection left no transitions
    transition_data <- shiny::reactive({
      if (input$apply_btn == 0) {
        purrr::pluck(cache, "workforce", "workforce_transition")
      } else {
        govhr::compute_transition(
          workforce_filtered(),
          id_col = input$id_col,
          group_col = input$group_filter
        ) |>
          dplyr::collect()
      }
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 1. transitions over time
    output$transition_trend_plot <- plotly::renderPlotly({
      validate_transitions(
        transition_data()
      )

      # compute_transition() returns one row per origin, destination and date,
      # so moves are summed from `transitions` rather than counted by row
      plotly::ggplotly(
        transition_data() |>
          govhr::fastcount(ref_date, wt = transitions, name = "transition") |>
          govhr::plot_trend(
            group_col = "ref_date",
            y_col = "transition",
            y_label = "Transitions"
          )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. transition network
    output$transition_network_plot <- ggiraph::renderGirafe({
      validate_transitions(
        transition_data()
      )

      govhr::plot_transition(
        transition_data(),
        weight_col = "transitions",
        interactive = TRUE
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}

#' Run the workforce transition panel on its own
#'
#' Launches the transition panel of the workforce tab by itself, for developing
#' and testing it.
#'
#' @inheritParams workforce_transition_server
#'
#' @returns A Shiny app object.
#'
#' @importFrom shiny shinyApp
#' @keywords internal
run_workforce_transition_app <- function(.data, cache) {
  shiny::shinyApp(
    ui = workforce_transition_ui("transition", .data),
    server = function(input, output, session) {
      workforce_transition_server("transition", .data, cache)
    }
  )
}
