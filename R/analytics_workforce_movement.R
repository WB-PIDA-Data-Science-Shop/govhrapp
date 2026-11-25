#' Workforce movement panel UI
#'
#' Sidebar controls, a trend and by-group plots of hires, separations or the
#' replacement rate, and a table comparing hires or separations with the whole
#' workforce.
#'
#' @param id Character. Module namespace ID.
#' @param .data Personnel data, as `workforce_data` in
#'   [run_govhrapp_analytics()].
#'
#' @returns A Shiny UI definition.
#'
#' @import bslib
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @keywords internal
workforce_movement_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!default_ui_controls(id, .data),
      shiny::selectInput(
        shiny::NS(id, "movement_type"),
        label = "Movement",
        choices = c(
          "Hires" = "hire",
          "Separations" = "separation",
          "Replacement rate" = "replacement"
        )
      ),
      shiny::selectInput(
        shiny::NS(id, "measurement_type"),
        label = "Measure",
        choices = c("Count" = "count", "Rate" = "rate")
      ),
      shiny::actionButton(
        shiny::NS(id, "apply_btn"),
        "Apply selection",
        icon = shiny::icon("play")
      )
    ),

    # plot 1. counts and rates over time
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Movements over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Hires are personnel active on a reference date but not on the previous one. Separations are personnel active on the previous reference date but not on this one, other than retirees, who are shown in the Retirement tab. Rates divide hires or separations by the previous reference date's active headcount. The replacement rate is the ratio of hires to separation: above 1, hires outnumber separations. Select a group to compare subgroups.",
          title = "Movements over time",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "movement_trend"))
    ),

    # table 1. demographic characteristics of movers vs. general pop.
    shiny::uiOutput(shiny::NS(id, "movement_profile")),

    bslib::layout_columns(
      col_widths = c(6, 6),
      # plot 2. counts/rates by group
      bslib::card(
        full_screen = TRUE,
        fillable = FALSE,
        bslib::card_header(
          "Movements by group",
          bslib::popover(
            bsicons::bs_icon("info-circle-fill"),
            "Count or rate on the latest reference date in the selected date range, by subgroup.",
            title = "Movements by group",
            placement = "left"
          ),
          class = "d-flex justify-content-between"
        ),
        plotly::plotlyOutput(shiny::NS(id, "movement_cross_section")),
        min_height = "450px"
      ),
      # plot 3. growth rate by group
      bslib::card(
        full_screen = TRUE,
        fillable = FALSE,
        bslib::card_header(
          "Growth in movements by group",
          bslib::popover(
            bsicons::bs_icon("info-circle-fill"),
            "Percentage change in the count or rate from the first to the last reference date in the selected date range, by subgroup.",
            placement = "left"
          ),
          class = "d-flex justify-content-between"
        ),
        plotly::plotlyOutput(shiny::NS(id, "movement_growth")),
        min_height = "450px"
      )
    )
  )
}
#' Workforce movement panel server
#'
#' Renders the movement plots and profile table, from the cache until the user
#' applies a selection.
#'
#' @inheritParams workforce_movement_ui
#' @param cache List of precomputed summaries from [build_analytics_cache()].
#'
#' @returns Called for its side effect of rendering the module's outputs.
#'
#' @import bslib
#' @import shiny
#' @importFrom dplyr all_of collect filter mutate select
#' @importFrom ggplot2 geom_hline
#' @importFrom govhr compute_growth compute_movement plot_bar_growth plot_bar_total plot_movement scale_plot_height
#' @importFrom gt render_gt
#' @importFrom gtsummary as_gt modify_header tbl_summary
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom purrr pluck
#' @keywords internal
workforce_movement_server <- function(id, .data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    data_filtered <- shiny::reactive({
      filter_data(
        .data,
        group_filter = input$group_filter,
        subgroup_filter = input$subgroup_filter,
        date_range = input$date_range
      )
    })

    # compute_movement() returns every movement and measurement type at once,
    # so switching between them reuses this collected aggregate rather than
    # re-running the query. govhr's scale_plot_height() and compute_growth()
    # also need it in memory
    movement_summary <- shiny::reactive({
      if (input$apply_btn == 0) {
        purrr::pluck(cache, "workforce", "workforce_movement")
      } else {
        govhr::compute_movement(
          data_filtered(),
          group_cols = group_col_to_null(input$group_filter)
        ) |>
          dplyr::collect()
      }
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # the first date has no earlier date to count hires and separations
    # against, and a group with nobody active on the previous date has no
    # separations, so plots 2 and 3 drop the rows missing the selected measure
    measure_summary <- shiny::reactive({
      measure_col <- movement_measure_col(
        input$movement_type,
        input$measurement_type
      )

      movement_summary() |>
        dplyr::filter(!is.na(.data[[measure_col]]))
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 1. counts/rates over time
    output$movement_trend <- plotly::renderPlotly({
      if (input$movement_type == "replacement") {
        plot_movement_trend(
          movement_summary(),
          y_col = "replacement_rate",
          y_label = "Replacement rate",
          group_col = input$group_filter
        ) +
          ggplot2::geom_hline(
            yintercept = 1,
            linetype = "dashed",
            color = "#004181"
          )
      } else {
        govhr::plot_movement(
          movement_summary(),
          movement_type = input$movement_type,
          measurement_type = input$measurement_type,
          group_col = group_col_to_null(input$group_filter)
        )
      }
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. counts/rates by group
    output$movement_cross_section <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$group_filter != "ref_date", "Select a group to see this chart.")
      )

      cross_section_data <- measure_summary() |>
        dplyr::filter(.data[["ref_date"]] == max(.data[["ref_date"]]))

      plotly::ggplotly(
        govhr::plot_bar_total(
          cross_section_data,
          group_col = input$group_filter,
          x_col = movement_measure_col(
            input$movement_type,
            input$measurement_type
          ),
          x_label = movement_labels[[input$movement_type]]
        ),
        height = govhr::scale_plot_height(cross_section_data)
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth rate by group
    output$movement_growth <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$group_filter != "ref_date", "Select a group to see this chart.")
      )

      growth_data <- measure_summary() |>
        govhr::compute_growth(
          group_col = input$group_filter,
          measure_col = movement_measure_col(
            input$movement_type,
            input$measurement_type
          )
        )

      plotly::ggplotly(
        govhr::plot_bar_growth(growth_data, group_col = input$group_filter),
        height = govhr::scale_plot_height(growth_data)
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # table 1. demographic characteristics of movers vs. general pop.
    output$movement_profile <- shiny::renderUI({
      shiny::req(input$movement_type)

      if (!input$movement_type %in% c("hire", "separation")) {
        return(NULL)
      }

      bslib::card(
        bslib::card_header(
          sprintf(
            "Demographic profile of %s",
            tolower(movement_labels[[input$movement_type]])
          ),
          bslib::popover(
            bsicons::bs_icon("info-circle-fill"),
            sprintf(
              "Age, gender, education and employment status of %s, compared with everyone else on the same reference dates. Age is measured as of today.",
              tolower(movement_labels[[input$movement_type]])
            ),
            title = "Demographic profile",
            placement = "left"
          ),
          class = "d-flex justify-content-between"
        ),
        gt::render_gt({
          shiny::req(input$movement_type %in% c("hire", "separation"))
          
          if (input$apply_btn == 0) {
            purrr::pluck(cache, "workforce", "workforce_movement_profile")
          } else {
          data_filtered() |>
            render_movement_profile(input$movement_type)
          }
        })
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}
