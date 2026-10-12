#' Workforce Movement UI
#'
#' Sidebar controls, trend and by-group plots, and a mover profile table for
#' recruitment, separation and turnover.
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame containing personnel data.
#'
#' @return A Shiny UI definition.
#'
#' @import bslib
#' @import shiny
#' @importFrom plotly plotlyOutput
workforce_movement_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!default_ui_controls(.data, id),
      shiny::selectInput(
        shiny::NS(id, "movement_type"),
        label = "Select type of movement:",
        choices = c(
          "Recruitment" = "hire",
          "Separation" = "separation",
          "Replacement" = "replacement"
        )
      ),
      shiny::selectInput(
        shiny::NS(id, "measurement_type"),
        label = "Select type of measurement:",
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
          "Hires are personnel active in a period but not in the previous one. Separations are personnel active in a period but not in the next one, for any reason including retirement. Rates divide hires or separations by the active headcount in the same period. The replacement rate is the ratio of hires to separations: above 1, more personnel join than leave.",
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
          "Counts/rates by group",
          bslib::popover(
            bsicons::bs_icon("info-circle-fill"),
            "Counts and rates by group. The counts and rates are computed for the latest available year in the selected time frame.",
            title = "Counts/rates by group",
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
          "Growth rate by group",
          bslib::popover(
            bsicons::bs_icon("info-circle-fill"),
            "Growth rate with respect to first reference date, by group.",
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
#' Workforce Movement Server
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame containing personnel data.
#' @param cache List of pre-computed summaries from [build_analytics_cache()].
#'
#' @return A Shiny module server function.
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
#' @importFrom stringr str_to_title
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

    # the first date has no hires and the last no separations, so plots 2 and 3
    # drop those dates for the selected measure only
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
          group_cols = group_col_to_null(input$group_filter)
        )
      }
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. counts/rates by group
    output$movement_cross_section <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$group_filter != "ref_date", "Please select a group.")
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
          x_label = stringr::str_to_title(input$movement_type)
        ),
        height = govhr::scale_plot_height(cross_section_data)
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth rate by group
    output$movement_growth <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$group_filter != "ref_date", "Please select a group.")
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
            "Demographic characteristics of %ss vs. general population",
            input$movement_type
          ),
          bslib::popover(
            bsicons::bs_icon("info-circle-fill"),
            sprintf(
              "Demographic characteristics of %ss vs. general population. The table shows the distribution of demographic characteristics for the selected movement type compared to the overall workforce.",
              input$movement_type
            ),
            title = "Demographic characteristics",
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
