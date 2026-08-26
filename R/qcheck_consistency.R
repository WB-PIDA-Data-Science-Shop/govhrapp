#' Consistency UI Module
#'
#' UI components for the consistency section of the quality control dashboard.
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
consistency_ui <- function(id, est_data, personnel_data, contract_data) {
  # coverage per module
  value_boxes <- list(
    uiOutput(
      NS(id, "consistency_est")
    ),
    uiOutput(
      NS(id, "consistency_personnel")
    ),
    uiOutput(
      NS(id, "consistency_contract")
    )
  )

  bslib::layout_columns(
    fillable = FALSE,
    col_widths = 12,
    
    # 1. value boxes for coverage metrics
    bslib::card(
      bslib::card_header(
        "Consistency: Overview",
        bslib::popover(
            bsicons::bs_icon("info-circle-fill"),
            "Computed as the global average of consistency, at the record and value levels, in each module.",
            title = "Consistency by module",
            placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      layout_column_wrap(
        fill = FALSE,
        !!!value_boxes
      )
    ),
    
    # 2. consistency by module
    bslib::navset_underline(
      bslib::nav_panel(
        title = "Establishment",
        consistency_panel_ui(NS(id, "est"), est_data)
      ),
      bslib::nav_panel(
        title = "Personnel",
        consistency_panel_ui(NS(id, "personnel"), personnel_data)
      ),
      bslib::nav_panel(
        title = "Contract",
        consistency_panel_ui(NS(id, "contract"), contract_data)
      )
    )
  )
}

#' Consistency Server Module
#'
#' Server logic for the consistency section, processing and visualizing whether data is consistent.
#'
#' @param id Character string. The module namespace ID.
#' @param est_data Data frame. The establishment-level data.
#' @param personnel_data Data frame. The personnel-level data.
#' @param contract_data Data frame. The contract-level data.
#' @param cache A list of data frames. Precomputed consistency data for the default grouping (ref_date).
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @import shiny
#'
#' @keywords internal
consistency_server <- function(id, est_data, personnel_data, contract_data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    # 1. value boxes for coverage metrics
    output$consistency_est <- render_consistency_box(
      est_data,
      id_col = "est_id",
      value_cols = "est_name_native",
      "Establishments",
      "building"
    )

    output$consistency_personnel <- render_consistency_box(
      personnel_data,
      id_col = "personnel_id",
      value_cols = c("birth_date"),
      "Personnel",
      "people-fill"
    )

    output$consistency_contract <- render_consistency_box(
      contract_data,
      id_col = "contract_id",
      value_cols = c("contract_type"),
      "Contracts",
      "file-text-fill"
    )

    # per-dataset panel sub-modules
    consistency_panel_server("est", est_data, cache = cache$est)
    consistency_panel_server("personnel", personnel_data, cache = cache$personnel)
    consistency_panel_server("contract",  contract_data, cache = cache$contract)
  })
}


#' Consistency Panel UI
#'
#' @param id Character string. The module namespace ID.
#' @param .data Data frame. The data to be used in the consistency panel.
#'
#' @import shiny
#' @importFrom bslib layout_sidebar sidebar card card_header tooltip
#' @importFrom bsicons bs_icon
#' @importFrom shinyWidgets materialSwitch
#'
#' @return A Shiny UI object representing the consistency panel.
consistency_panel_ui <- function(id, .data) {
  accordion_controls <- bslib::accordion(
    accordion_panel(
      "Filters",
      icon = bsicons::bs_icon("filter"),
      !!!default_ui_controls(.data, id)
    ),
    accordion_panel(
      "Additional controls",
      icon = bsicons::bs_icon("bar-chart"),
      shiny::selectInput(
        shiny::NS(id, "type_plot"),
        "Select type of consistency:",
        choices = c(
          "Record" = "record",
          "Value" = "value"
        ),
        selected = "record"
      ),
      # conditionally show the value column selection only when "Value" plot type is selected
      shiny::conditionalPanel(
        condition = sprintf(
          "input['%s'] === 'value'",
          shiny::NS(id, "type_plot")
        ),
        shiny::selectInput(
          shiny::NS(id, "value_col"),
          "Select value column:",
          choices = identify_group_choices(.data),
          selected = identify_group_choices(.data)[1]
        )
      ),
      shinyWidgets::materialSwitch(
        shiny::NS(id, "toggle_growth"),
        label = "Switch to baseline index",
        value = FALSE
      )
    )
  )

  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      accordion_controls,
      shiny::actionButton(
        shiny::NS(id, "apply_btn"),
        "Apply selection",
        icon = shiny::icon("play")
      )
    ),

    # plot 1. consistency over time
    bslib::card(
      full_screen = TRUE,
      fillable = FALSE,
      bslib::card_header(
        "Consistency over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Computed as the global average of consistency, at the record level, in each module.",
          title = "Consistency over time",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "consistency_panel"),
        height = "350px"
      )
    ),

    # plot 2. heatmap consistency by group
    bslib::card(
      full_screen = TRUE,
      fillable = FALSE,
      bslib::card_header(
        "Consistency heatmap by group",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Computed as the global average of consistency, at the value level, in each module, by variable and group.",
          title = "Consistency heatmap by group",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "consistency_heatmap"),
        height = "400px"
      )
    )
  )
}

#' Consistency Panel Server
#'
#' @param id Character string. Sub-module ID.
#' @param .data A dataframe. Input dataset for the sub-module.
#' @param cache A list of dataframes. Precomputed consistency data for the default grouping (ref_date).
#'
#' @import shiny
#' @importFrom plotly renderPlotly
#' @importFrom shinyWidgets updatePickerInput pickerOptions
#' @importFrom dplyr filter
#'
#' @return A set of Shiny outputs for the consistency panel.
consistency_panel_server <- function(id, .data, cache) {
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

    # plot 1. consistency over time
    output$consistency_panel <- plotly::renderPlotly({
      id_col <- switch(
        id,
        "est" = "est_id",
        "personnel" = "personnel_id",
        "contract" = "contract_id"
      )

      # compute and cache the appropriate data for selected plot type
      data_consistency_panel <- shiny::reactive({
        # use cache if default (group filter input is ref_date), otherwise compute on filtered data
        if (input$group_filter == "ref_date") {
          cache
        } else {
          if (input$type_plot == "record") {
            govhr::compute_record_consistency(
              data_filtered(),
              id_col = id_col,
              group_cols = input$group_filter
            )
          } else {
            govhr::compute_value_consistency(
              data_filtered(),
              id_col = id_col,
              value_col = input$value_col,
              group_cols = input$group_filter
            )
          }
        }
        })

      plot_consistency_trend(
        data_consistency_panel(),
        id_col = id_col,
        type_plot = input$type_plot,
        group = input$group_filter,
        value_col = input$value_col,
        toggle_growth = input$toggle_growth
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. heatmap consistency by group
    output$consistency_heatmap <- plotly::renderPlotly({
      id_col <- switch(
        id,
        "est" = "est_id",
        "personnel" = "personnel_id",
        "contract" = "contract_id"
      )

      plot_consistency_heatmap(
        data_filtered(),
        id_col = id_col,
        group = input$group_filter
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}

run_consistency_app <- function(
  est_data,
  personnel_data,
  contract_data,
  ...
) {
  theme = bslib::bs_theme(
    bootswatch = "litera"
  )

  ui <- consistency_ui("test", est_data, personnel_data, contract_data)

  server <- function(input, output, session) {
    consistency_server("test", est_data, personnel_data, contract_data)
  }

  shiny::shinyApp(ui, server, ...)
}
