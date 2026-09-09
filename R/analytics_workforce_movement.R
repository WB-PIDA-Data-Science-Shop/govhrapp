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
        choices = c("Recruitment" = "hire", "Separation" = "fire", "Turnover" = "turnover")
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
          "Hires and fires are computed as the number of new hires and fires in each period. Turnover is computed as the ratio of hires to fires and retirements. The rate is computed as the number of new hires divided by the total workforce at the beginning of each period.",
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
#' @importFrom dplyr all_of mutate select
#' @importFrom govhr classify_personnel_event compute_workforce_movement
#' @importFrom gt render_gt
#' @importFrom gtsummary as_gt modify_header tbl_summary
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom purrr pluck
#' @importFrom stats na.omit
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

    # all three plots read the same aggregate, so compute it once per apply
    movement_summary <- shiny::reactive({
      if (input$apply_btn == 0) {
        purrr::pluck(cache, "workforce", "workforce_movement")
      } else {
        govhr::compute_workforce_movement(
          .data = data_filtered(),
          movement_type = input$movement_type,
          measurement_type = input$measurement_type,
          group_cols = input$group_filter
        )
      }
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 1. hiring counts/rates over time
    output$movement_trend <- plotly::renderPlotly({
      govhr::plot_movement(
        movement_summary(),
        movement_type = input$movement_type,
        measurement_type = input$measurement_type,
        group_col = input$group_filter
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. counts/rates by group
    output$movement_cross_section <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$group_filter != "ref_date", "Please select a group.")
      )

      cross_section_data <- movement_summary() |>
        stats::na.omit() |>
        dplyr::filter(.data[["ref_date"]] == max(.data[["ref_date"]]))

      plotly::ggplotly(
        govhr::plot_bar_total(
          cross_section_data,
          group_col = input$group_filter,
          x_col = "indicator",
          x_label = stringr::str_to_title(input$movement_type)
        ),
        height = scale_plot_height(cross_section_data)
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth rate by group
    output$movement_growth <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$group_filter != "ref_date", "Please select a group.")
      )

      growth_data <- movement_summary() |>
        stats::na.omit() |>
        compute_growth_summary(
          group_col = input$group_filter,
          measure_col = "indicator"
        )

      plotly::ggplotly(
        govhr::plot_bar_growth(growth_data, group_col = input$group_filter),
        height = scale_plot_height(growth_data)
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # table 1. demographic characteristics of movers vs. general pop.
    output$movement_profile <- shiny::renderUI({
      shiny::req(input$movement_type)

      if (!input$movement_type %in% c("hire", "fire")) {
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
          movement_data <- data_filtered()
          ref_dates <- movement_data[["ref_date"]]

          govhr::classify_personnel_event(
            .data = movement_data,
            id_col = "personnel_id",
            event_type = input$movement_type,
            start_date = min(ref_dates),
            end_date = max(ref_dates),
            status_col = "employment_status",
            freq = guess_date_frequency(movement_data)
          ) |>
            dplyr::mutate(
              age = as.numeric(
                difftime(Sys.Date(), birth_date, units = "days")
              ) /
                365.25
            ) |>
            dplyr::select(-dplyr::all_of("birth_date")) |>
            gtsummary::tbl_summary(
              by = "type_event",
              include = -c("personnel_id", "ref_date"),
              label = list(
                "gender" = "Gender",
                "educat7" = "Education Level",
                "employment_status" = "Employment Status",
                "age" = "Age"
              )
            ) |>
            gtsummary::modify_header(
              label = "**Variable**",
              stat_1 = sprintf(
                "**New %ss**",
                stringr::str_to_title(input$movement_type)
              ),
              stat_2 = "**General Population**"
            ) |>
            gtsummary::as_gt()
        })
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}
