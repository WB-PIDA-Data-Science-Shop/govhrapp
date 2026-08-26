#' coverage UI Module
#'
#' UI components for the coverage section of the quality control dashboard.
#'
#' @param id Character string. The module namespace ID.
#' @param est_data Data frame. The establishment-level data.
#' @param personnel_data Data frame. The personnel-level data.
#' @param contract_data Data frame. The contract-level data.
#'
#' @return A Shiny UI object containing coverage analysis cards.
#'
#' @import shiny
#' @import bslib
#'
#' @keywords internal
coverage_ui <- function(id, est_data, personnel_data, contract_data) {
  # coverage per module
  value_boxes <- list(
    uiOutput(
      NS(id, "coverage_est")
    ),
    uiOutput(
      NS(id, "coverage_personnel")
    ),
    uiOutput(
      NS(id, "coverage_contract")
    )
  )

  bslib::layout_columns(
    fillable = FALSE,
    col_widths = 12,

    # 1. value boxes for coverage metrics
    bslib::card(
      bslib::card_header(
        "Coverage: Overview",
        bslib::popover(
            bsicons::bs_icon("info-circle-fill"),
            "Coverage is computed as the average of the proportion of complete values for each variable in the module. For example, if a module has 2 variables and 50 percent of values are missing for each, the global coverage is 50%.",
            title = "Coverage by module",
            placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      layout_column_wrap(
        fill = FALSE,
        !!!value_boxes
      )
    ),
    
    # 2. coverage by module
    bslib::navset_underline(
      bslib::nav_panel(
        title = "Establishment",
        coverage_panel_ui(NS(id, "est"), est_data)
      ),
      bslib::nav_panel(
        title = "Personnel",
        coverage_panel_ui(NS(id, "personnel"), personnel_data)
      ),
      bslib::nav_panel(
        title = "Contract",
        coverage_panel_ui(NS(id, "contract"), contract_data)
      )
    )
  )
}

#' Coverage Server Module
#'
#' Server logic for the coverage section, processing and visualizing missing data patterns.
#'
#' @param id Character string. The module namespace ID.
#' @param est_data Data frame. The establishment-level data.
#' @param personnel_data Data frame. The personnel-level data.
#' @param contract_data Data frame. The contract-level data.
#' @param cache A list of dataframes. It caches pre-computed coverage results. Passed on to `coverage_panel_server()`.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @import shiny
#'
#' @keywords internal
coverage_server <- function(id, est_data, personnel_data, contract_data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    # 1. value boxes for coverage metrics
    output$coverage_est <- render_coverage_box(
      est_data,
      "Establishments",
      "building"
    )
    
    output$coverage_contract <- render_coverage_box(
      contract_data,
      "Contracts",
      "file-text-fill"
    )
    
    output$coverage_personnel <- render_coverage_box(
      personnel_data,
      "Personnel",
      "people-fill"
    )

    # per-dataset panel sub-modules
    coverage_panel_server("est",       est_data,        cache = cache$est)
    coverage_panel_server("personnel", personnel_data,  cache = cache$personnel)
    coverage_panel_server("contract",  contract_data,   cache = cache$contract)
  })
}

#' Coverage Panel UI
#'
#' @param id Character string. The module namespace ID.
#' @param .data Data frame. The data to be used in the coverage panel.
#'
#' @return A Shiny UI object representing the coverage panel.
coverage_panel_ui <- function(id, .data) {
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

    # plot 1. coverage over time
    bslib::card(
      full_screen = TRUE,
      fillable = FALSE,
      bslib::card_header(
        "Coverage over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          placement = "left",
          title = "Coverage over time",
          "Computed as the average proportion of non-missing values for all variables over time."
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "coverage_panel"),
        height = "350px"
      )
    ),

    # plot 2. coverage by variable
    bslib::card(
      full_screen = TRUE,
      fillable = FALSE,
      bslib::card_header(
        "Coverage by variable",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          placement = "left",
          title = "Coverage by variable",
          "Computed as the proportion of non-missing values for each variable."
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "coverage_by_variable"),
        height = "350px"
      )
    ),

    # plot 3. heatmap coverage by group
    bslib::card(
      full_screen = TRUE,
      fillable = FALSE,
      bslib::card_header(
        "Coverage heatmap by group",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Coverage, by variable and group.",
          title = "Coverage heatmap by group",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "coverage_heatmap"),
        height = "400px"
      )
    )
  )
}

#' Coverage Panel Server
#'
#' Server logic for individual coverage panels.
#'
#' @param id Character string. Sub-module ID matching the one used in
#'   [coverage_panel_ui()].
#' @param .data Data frame for the panel (e.g., establishment, personnel, or contract).
#' @param cache A list for caching pre-computed results.
#'
#' @return A set of Shiny outputs for the coverage panel.
coverage_panel_server <- function(id, .data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)
      }
    })

    data_filtered <- shiny::reactive({
      filter_data(
        .data,
        group_filter = input$group_filter,
        subgroup_filter = input$subgroup_filter,
        date_range = input$date_range
      )
    })

    # if default (group filter input is ref_date), use cache data
    coverage_trend_data <- shiny::reactive({
      if (input$group_filter == "ref_date") {
        data <- cache
        if (!is.null(input$date_range)) {
          data <- data |>
            dplyr::filter(
              .data[["ref_date"]] >= input$date_range[1],
              .data[["ref_date"]] <= input$date_range[2]
            )
        }
        data
      } else {
        data_filtered()
      }
    })

    # plot 1. coverage over time
    output$coverage_panel <- plotly::renderPlotly({
      plot_coverage_trend(
        coverage_trend_data(),
        group = input$group_filter,
        toggle_growth = input$toggle_growth
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. coverage by variable
    output$coverage_by_variable <- plotly::renderPlotly({
      plot_coverage_bar(data_filtered())
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. coverage heatmap by group
    output$coverage_heatmap <- plotly::renderPlotly({
      plot_coverage_heatmap(
        data_filtered(),
        group = input$group_filter
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}

run_coverageapp <- function(
  est_data,
  personnel_data,
  contract_data,
  ...
) {
  ui <- coverage_ui("test", est_data, contract_data, personnel_data)

  server <- function(input, output, session) {
    coverage_server("test", est_data, contract_data, personnel_data)
  }

  shiny::shinyApp(ui, server, ...)
}
