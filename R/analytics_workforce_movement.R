#' Workforce Movement UI
#'
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing personnel data.
#'
#' @import shiny
#' @import bslib
#' @importFrom plotly plotlyOutput
#'
#' @return A Shiny UI function for the workforce movement module.
workforce_movement_ui <- function(
  id,
  .data
) {
  # move profile table right under movements over time
  # organize the outputs into two buckets: high-level and deep-dive
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!default_ui_controls(.data, id),
      shiny::selectInput(
        shiny::NS(id, "movement_type"),
        label = "Select type of movement:",
        choices = c("Hire" = "hire", "Fire" = "fire", "Turnover" = "turnover")
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
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing personnel data.
#' @param cache A list containing cached data for the module.
#'
#' @import shiny
#' @import bslib
#' @importFrom plotly renderPlotly
#' @importFrom gt render_gt
#' @importFrom gtsummary tbl_summary modify_header as_gt
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom dplyr filter
#' @importFrom govhr compute_workforce_movement classify_personnel_event
#' @importFrom purrr pluck
#'
#' @return A Shiny server function for the workforce movement module.
workforce_movement_server <- function(
  id,
  .data,
  cache
) {
  shiny::moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    data_filtered <- shiny::reactive({
      .data |>
          filter_data(
            group_filter = input$group_filter,
            subgroup_filter = input$subgroup_filter,
            date_range = input$date_range
          )
    })

    # plot 1. hiring counts/rates over time
    output$movement_trend <- plotly::renderPlotly({
      plot_data <- if(input$apply_btn == 0) {
        cache |>
          purrr::pluck("workforce", "workforce_movement")
      } else {
        govhr::compute_workforce_movement(
          .data = data_filtered(),
          movement_type = input$movement_type,
          measurement_type = input$measurement_type,
          group_cols = input$group_filter
        )
      }

      plot_movement(
        plot_data,
        movement_type = input$movement_type,
        measurement_type = input$measurement_type,
        group_cols = input$group_filter
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. counts/rates by group
    output$movement_cross_section <- plotly::renderPlotly({
      validate(
        need(input$group_filter != "ref_date", "Please select a group.")
      )

      cross_section_data <- govhr::compute_workforce_movement(
        .data = data_filtered(),
        movement_type = input$movement_type,
        measurement_type = input$measurement_type,
        group_cols = input$group_filter
      ) |>
        na.omit() |>
        dplyr::filter(
          .data[["ref_date"]] == max(.data[["ref_date"]])
        )

      n_groups <- nrow(cross_section_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_total(
          cross_section_data,
          group = input$group_filter,
          x_col = "indicator",
          x_label = stringr::str_to_title(input$movement_type)
        ),
        height = plot_height
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth rate by group
    output$movement_growth <- plotly::renderPlotly({
      validate(
        need(input$group_filter != "ref_date", "Please select a group.")
      )

      turnover_data <- govhr::compute_workforce_movement(
        .data = data_filtered(),
        movement_type = input$movement_type,
        measurement_type = input$measurement_type,
        group_cols = input$group_filter
      ) |>
        na.omit()

      growth_data <- compute_growth_summary(
        turnover_data,
        measure_col = "indicator",
        group = input$group_filter
      )

      n_groups <- nrow(growth_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_growth(growth_data, group = input$group_filter),
        height = plot_height
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # table 1. demographic characteristics of movers vs. general pop.
    output$movement_profile <- renderUI({
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
          profile_data <- govhr::classify_personnel_event(
            .data = data_filtered(),
            id_col = "personnel_id",
            event_type = input$movement_type,
            start_date = min(data_filtered()[["ref_date"]]),
            end_date = max(data_filtered()[["ref_date"]]),
            status_col = "employment_status",
            freq = guess_date_frequency(data_filtered())
          ) |>
            dplyr::mutate(
              age = as.numeric(difftime(
                Sys.Date(),
                birth_date,
                units = "days"
              )) /
                365.25
            ) |>
            dplyr::select(-all_of("birth_date"))

          profile_data |>
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
        }) |>
          bindEvent(input$apply_btn, ignoreNULL = FALSE)
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}
