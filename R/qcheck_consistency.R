#' Consistency tab UI
#'
#' Consistency boxes for each table, and a consistency panel for each of the
#' establishment, personnel and contract tables.
#'
#' @param id Character. Module namespace ID.
#' @inheritParams run_govhrapp_qcheck
#'
#' @returns A Shiny UI definition.
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

#' Consistency tab server
#'
#' Renders the consistency boxes and runs a consistency panel server for each
#' table.
#'
#' @param id Character. Module namespace ID.
#' @inheritParams run_govhrapp_qcheck
#' @param cache List of precomputed record consistency by reference date, with
#'   elements `est`, `personnel` and `contract`, as in the `consistency`
#'   element of [build_qcheck_cache()].
#'
#' @returns Called for its side effect of rendering the module's outputs.
#'
#' @import shiny
#'
#' @keywords internal
consistency_server <- function(id, est_data, personnel_data, contract_data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    # 1. value boxes for consistency metrics
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
    consistency_panel_server("est", est_data, cache = cache[["est"]])
    consistency_panel_server("personnel", personnel_data, cache = cache[["personnel"]])
    consistency_panel_server("contract",  contract_data, cache = cache[["contract"]])
  })
}


#' Consistency panel UI
#'
#' Sidebar controls and consistency plots over time and by variable, for one
#' table.
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame. The establishment, personnel or contract table the
#'   panel describes.
#'
#' @import shiny
#' @importFrom bslib layout_sidebar sidebar card card_header tooltip
#' @importFrom bsicons bs_icon
#' @importFrom shinyWidgets materialSwitch
#'
#' @returns A Shiny UI definition.
#'
#' @keywords internal
consistency_panel_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!default_ui_controls(id, .data),
      shiny::selectInput(
        shiny::NS(id, "type_consistency"),
        "Consistency type",
        choices = c(
          "Record" = "record",
          "Value" = "value"
        ),
        selected = "record"
      ),
      # the column only matters for value consistency
      shiny::conditionalPanel(
        condition = sprintf(
          "input['%s'] === 'value'",
          shiny::NS(id, "type_consistency")
        ),
        shiny::selectInput(
          shiny::NS(id, "value_col"),
          "Column",
          choices = identify_group_choices(.data),
          selected = identify_group_choices(.data)[1]
        )
      ),
      shinyWidgets::materialSwitch(
        shiny::NS(id, "toggle_growth"),
        label = "Show as index (first date = 100)",
        value = FALSE
      ),
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

    # plot 2. heatmap consistency by group (only display if the type of consistency chosen is value)
    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] === 'value'",
        shiny::NS(id, "type_consistency")
      ),
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
  )
}

#' Consistency panel server
#'
#' Renders the consistency plots of one table, from the cache until the user
#' applies a filter.
#'
#' @param id Character. Module namespace ID, matching the one passed to
#'   [consistency_panel_ui()].
#' @inheritParams consistency_panel_ui
#' @param cache Data frame. Precomputed record consistency of `.data` by
#'   reference date.
#'
#' @import shiny
#' @importFrom dplyr bind_rows collect mutate
#' @importFrom govhr compute_record_consistency compute_value_consistency
#'   plot_consistency_heatmap plot_consistency_trend
#' @importFrom plotly renderPlotly
#' @importFrom purrr map
#' @importFrom shinyWidgets updatePickerInput pickerOptions
#' @importFrom scales breaks_pretty
#'
#' @returns Called for its side effect of rendering the module's outputs.
#'
#' @keywords internal
consistency_panel_server <- function(id, .data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    id_col <- switch(
      id,
      "est" = "est_id",
      "personnel" = "personnel_id",
      "contract" = "contract_id"
    )

    data_filtered <- shiny::reactive({
      filter_data(
        .data,
        group_filter = input$group_filter,
        subgroup_filter = input$subgroup_filter,
        date_range = input$date_range
      )
    })

    # the cache holds record consistency by date, which the default selection
    # shows before the first apply
    consistency_trend <- shiny::reactive({
      if (input$apply_btn == 0) {
        return(cache)
      }

      group_cols <- unique(c(input$group_filter, "ref_date"))

      if (input$type_consistency == "record") {
        govhr::compute_record_consistency(
          data_filtered(),
          id_col = id_col,
          group_cols = group_cols
        )
      } else {
        govhr::compute_value_consistency(
          data_filtered(),
          id_col = id_col,
          value_col = input$value_col,
          group_cols = group_cols
        )
      }
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 1. consistency over time
    output$consistency_panel <- plotly::renderPlotly({
      govhr::plot_consistency_trend(
        consistency_trend(),
        group_col = input$group_filter,
        type_plot = input$type_consistency,
        toggle_growth = input$toggle_growth
      ) +
        scale_y_continuous(
          breaks = scales::breaks_pretty()
        )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. heatmap consistency by group
    output$consistency_heatmap <- plotly::renderPlotly({
      value_cols <- setdiff(
        colnames(data_filtered()),
        c(id_col, input$group_filter)
      )

      # the heatmap is drawn in memory, so each column's small summary is
      # collected and stacked there. stacking lazy tables would instead build
      # one query whose SQL grows with every column
      consistency_by_variable <- value_cols |>
        purrr::map(
          \(value_col) {
            govhr::compute_value_consistency(
              data_filtered(),
              id_col = id_col,
              value_col = value_col,
              group_cols = input$group_filter
            ) |>
              dplyr::collect() |>
              dplyr::mutate(variable = value_col)
          }
        ) |>
        dplyr::bind_rows()

      govhr::plot_consistency_heatmap(
        consistency_by_variable,
        group_col = input$group_filter
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}

#' Run the consistency tab on its own
#'
#' Launches the consistency tab of the quality check app by itself, for
#' developing and testing it.
#'
#' @inheritParams run_govhrapp_qcheck
#'
#' @returns A Shiny app object.
#'
#' @importFrom shiny shinyApp
#' @keywords internal
run_consistency_app <- function(
  est_data,
  personnel_data,
  contract_data,
  ...
) {
  cache <- build_qcheck_cache(est_data, personnel_data, contract_data)

  ui <- consistency_ui("consistency", est_data, personnel_data, contract_data)

  server <- function(input, output, session) {
    consistency_server(
      "consistency",
      est_data,
      personnel_data,
      contract_data,
      cache = cache[["consistency"]]
    )
  }

  shiny::shinyApp(ui, server, ...)
}
