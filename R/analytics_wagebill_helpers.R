#' Function to identify available wagebill measure choices from the data and dictionary
#' @param .data A data frame containing wagebill data.
#' 
#' @import dplyr
#' @importFrom purrr set_names
#' 
#' @return A named list of wagebill measure choices, where each element corresponds to a module and contains a named vector of variable IDs and their corresponding variable names.
identify_wagebill_choices <- function(.data) {
  available_cols <- names(.data)

  wagebill_choices <- govhr::dictionary |>
    dplyr::filter(
      .data[["variable_id"]] %in%
        available_cols &
        stringr::str_detect(.data[["variable_id"]], "salary|allowance")
    ) |>
    dplyr::summarise(
      choices = list(
        purrr::set_names(.data[["variable_id"]], .data[["variable_name"]])
      ),
      .by = .data[["module"]]
    ) |>
    dplyr::pull(.data[["choices"]], name = .data[["module"]])

  wagebill_choices
}

#' Function to create the UI for the wagebill overview module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#'
#' @import shiny
#' @import bslib
#' @importFrom shinyWidgets materialSwitch
#' @importFrom plotly plotlyOutput
#' @importFrom bsicons bs_icon
#' 
#' @return A Shiny module UI function for the wagebill overview module.
wagebill_overview_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = span("Controls", bsicons::bs_icon("sliders")),
      width = "300px",
      !!!ui_filter_controls(.data, id),
      # conditionally allow user to select wagebill measure if available
      shiny::selectInput(
        shiny::NS(id, "wagebill_measure"),
        "Type of Wage:",
        choices = identify_wagebill_choices(.data)
      ),
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
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Wage Bill Time Trends",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Wage bill trends over time. Choosing a group will add new trend lines, by group."
        )
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_panel"),
        height = "350px"
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        fillable = FALSE,
        bslib::card_header(
          "Total by group",
          bslib::popover(
            bsicons::bs_icon("info-circle-fill"),
            "Total amount, by group. Total refers to the latest available year in the selected time frame.",
            placement = "left"
          ),
          class = "d-flex justify-content-between"
        ),
        plotly::plotlyOutput(shiny::NS(id, "wagebill_cross_section")),
        min_height = "450px"
      ),
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
        plotly::plotlyOutput(shiny::NS(id, "wagebill_change")),
        min_height = "450px"
      )
    )
  )
}

#' Function to create the server logic for the wagebill overview module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#' 
#' @import shiny
#' @importFrom plotly renderPlotly
#' 
#' @return A Shiny module server function for the wagebill overview module.
wagebill_overview_server <- function(id, .data) {
  shiny::moduleServer(id, function(input, output, session) {
    # choice of cols
    wagebill_group_choices <- identify_group_choices(.data)

    update_group_filter_controls(.data, input, session)

    wagebill_filtered <- shiny::reactive({
      data <- .data

      if (input$group_filter != "ref_date") {
        data <- data |>
          dplyr::filter(
            .data[[input$group_filter]] %in% input$subgroup_filter
          )
      }

      data |>
        dplyr::filter(
          .data[["ref_date"]] >= input$date_range[1],
          .data[["ref_date"]] <= input$date_range[2]
        )
    })

    wagebill_summary <- shiny::reactive({
      out <- govhr::compute_time_trend(
        wagebill_filtered(),
        group = input$group_filter,
        measure_col = input$wagebill_measure
      )

      if (input$toggle_growth) {
        out <- govhr::rescale_baseline(out, group = input$group_filter)
      }

      out
    })

    wagebill_annual <- shiny::reactive({
      wagebill_filtered() |>
        govhr::compute_fastsummary(
          cols = input$wagebill_measure,
          fns = "sum",
          groups = c("ref_date", "country_code")
        )
    })

    # plot 1. panel
    output$wagebill_panel <- plotly::renderPlotly({
      plotly::ggplotly(
        plot_trend(
          wagebill_summary(),
          group = input$group_filter,
          toggle_growth = input$toggle_growth,
          y_label = "Wage Bill"
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. macro panel
    output$wagebill_fiscal <- renderPlotly({
      wagebill_fiscal <- wagebill_annual() |>
        mutate(
          year = lubridate::year(ref_date)
        ) |>
        left_join(
          govhr::macro_indicators,
          by = c("country_code", "year")
        ) |>
        mutate(
          ratio = .data[["value"]] / .data[[input$macroindicator_measure]] * 100
        )

      plot <- wagebill_fiscal |>
        ggplot2::ggplot(
          ggplot2::aes(x = .data[["ref_date"]], y = .data[["ratio"]])
        ) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::xlab("Time") +
        ggplot2::ylab("Ratio") +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(scale = 1)
        )

      plotly::ggplotly(plot)
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. total by group
    output$wagebill_cross_section <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Please select a group."
        )
      )

      cross_section_data <- govhr::compute_cross_section(
        wagebill_filtered(),
        group = input$group_filter,
        measure_col = input$wagebill_measure
      )

      n_groups <- nrow(cross_section_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_total(
          cross_section_data,
          group = input$group_filter,
          x_label = "Wage bill"
        ),
        height = plot_height
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth rate by group
    output$wagebill_change <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Please select a group."
        )
      )

      change_data <- govhr::compute_growth(
        wagebill_filtered(),
        group = input$group_filter,
        measure_col = input$wagebill_measure
      )

      n_groups <- nrow(change_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_growth(change_data, group = input$group_filter),
        height = plot_height
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}

#' Function to create the UI for the wagebill equity module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#' 
#' @import bslib
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom bsicons bs_icon
#' 
#' @return A Shiny module UI function for the wagebill equity module.
wagebill_equity_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = span("Controls", bsicons::bs_icon("sliders")),
      width = "300px",
      !!!ui_filter_controls(.data, id),
      shiny::selectInput(
        shiny::NS(id, "wagebill_measure"),
        "Type of Wage:",
        choices = identify_wagebill_choices(.data)
      ),
      shiny::actionButton(
        shiny::NS(id, "apply_btn"),
        "Apply selection",
        icon = shiny::icon("play")
      )
    ),
    # plot 1. wage distribution
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Wage Distribution",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Wage density distribution. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        bslib::popover(
          bsicons::bs_icon("gear"),
          shiny::radioButtons(
            inputId = shiny::NS(id, "plot_type"),
            label = "Plot type",
            choices = c("Histogram" = "histogram", "Cumulative" = "cumulative"),
            selected = "histogram"
          ),
          title = "Chart options",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_density"),
        height = "350px"
      )
    ),
    # plot 2. wage by decile
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Wage by Decile",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Wage distribution by decile. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_distribution"),
        height = "350px"
      )
    ),
    # plot 3. compression ratio
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Wage Compression Ratio (10th to 90th Percentile)",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Wage compression ratio between the 10th and 90th percentile. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_compression_ratio"),
        height = "350px"
      )
    )
  )
}

#' Function to create the server logic for the wagebill equity module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#' 
#' @import shiny
#' @importFrom plotly renderPlotly
#' @importFrom dplyr filter
#' 
#' @return A Shiny module server function for the wagebill equity module.
wagebill_equity_server <- function(id, .data) {
  shiny::moduleServer(id, function(input, output, session) {
    # choice of cols
    wagebill_group_choices <- identify_group_choices(.data)

    update_group_filter_controls(.data, input, session)

    wagebill_filtered <- shiny::reactive({
      data <- .data

      if (input$group_filter != "ref_date") {
        data <- data |>
          dplyr::filter(
            .data[[input$group_filter]] %in% input$subgroup_filter
          )
      }

      data |>
        dplyr::filter(
          .data[["ref_date"]] >= input$date_range[1],
          .data[["ref_date"]] <= input$date_range[2]
        )
    })

    # plot 1. wage density
    output$wagebill_density <- plotly::renderPlotly({
      wagebill_density <- wagebill_filtered() |>
        dplyr::filter(.data[["ref_date"]] == max(.data[["ref_date"]])) |>
        govhr::compute_density(
          group_col = input$group_filter,
          binwidth = 100,
          measure_col = input$wagebill_measure
        )

      plotly::ggplotly(
        plot_histogram(
          wagebill_density,
          plot_type = input$plot_type,
          group_col = input$group_filter
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. wage by decile
    output$wagebill_distribution <- plotly::renderPlotly({
      # filter latest ref_date
      latest_ref_date <- max(wagebill_filtered()[["ref_date"]])
      wagebill_filtered_latest <- wagebill_filtered() |>
        dplyr::filter(.data[["ref_date"]] == latest_ref_date)

      wagebill_distribution <- govhr::compute_quantile(
        wagebill_filtered_latest,
        group_cols = input$group_filter,
        measure_col = input$wagebill_measure,
        latest_measure = TRUE
      )

      n_groups <- nrow(wagebill_distribution)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_decile(
          wagebill_distribution,
          group_cols = input$group_filter
        ),
        height = plot_height
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. wage range between 10th and 90th percentile
    output$wagebill_compression_ratio <- plotly::renderPlotly({
      wagebill_compression_ratio <- govhr::compute_compression_ratio(
        wagebill_filtered(),
        group_cols = input$group_filter,
        measure_col = input$wagebill_measure,
        latest_measure = TRUE
      )

      n_groups <- nrow(wagebill_compression_ratio)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_compression_ratio(
          wagebill_compression_ratio,
          group_cols = input$group_filter
        ),
        height = plot_height
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}

#' Function to create the UI for the wagebill movement module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#' 
#' @import bslib
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom bsicons bs_icon
#' 
#' @return A Shiny module UI function for the wagebill movement module.
wagebill_movement_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = span("Controls", bsicons::bs_icon("sliders")),
      width = "300px",
      !!!ui_filter_controls(.data, id),
      shiny::selectInput(
        shiny::NS(id, "event_type"),
        "Type of Movement:",
        choices = c("Hire" = "hire", "Fire" = "fire"),
        selected = "hire"
      ),
      shiny::selectInput(
        shiny::NS(id, "wagebill_measure"),
        "Type of Wage:",
        choices = identify_wagebill_choices(.data)
      ),
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
    # plot 1. labor movement costs
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Labor Movement Costs",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Labor movement costs over time. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_movement"),
        height = "350px"
      )
    ),
    # plot 2. labor movement costs by group
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Labor Movement Costs by Group",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Labor movement costs by group. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_movement_by_group"),
        height = "350px"
      )
    ),
    # plot 3. growth in labor movement costs by group
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Growth in Labor Movement Costs by Group",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Growth in labor movement costs by group. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_movement_growth"),
        height = "350px"
      )
    )
  )
}

#' Function to create the server logic for the wagebill movement module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#' 
#' @import shiny
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom dplyr filter
#' 
#' @return A Shiny module server function for the wagebill movement module.
wagebill_movement_server <- function(id, .data) {
  shiny::moduleServer(id, function(input, output, session) {
    # choice of cols
    wagebill_group_choices <- identify_group_choices(.data)

    update_group_filter_controls(.data, input, session)

    wagebill_filtered <- shiny::reactive({
      data <- .data

      if (input$group_filter != "ref_date") {
        data <- data |>
          dplyr::filter(
            .data[[input$group_filter]] %in% input$subgroup_filter
          )
      }

      data |>
        dplyr::filter(
          .data[["ref_date"]] >= input$date_range[1],
          .data[["ref_date"]] <= input$date_range[2]
        )
    })

    # plot 1. labor movement costs
    output$wagebill_movement <- plotly::renderPlotly({
      labor_movement_data <- govhr::compute_movement_cost(
        wagebill_filtered(),
        event_type = input$event_type,
        measure_col = input$wagebill_measure,
        group_cols = input$group_filter
      )

      plotly::ggplotly(
        plot_trend(
          labor_movement_data,
          group = input$group_filter,
          toggle_growth = input$toggle_growth,
          y_col = "movement_cost",
          y_label = "Movement Costs"
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. labor movement costs by group
    output$wagebill_movement_by_group <- plotly::renderPlotly({
      validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Please select a group."
        )
      )

      labor_movement_data <- govhr::compute_movement_cost(
        wagebill_filtered(),
        event_type = input$event_type,
        measure_col = input$wagebill_measure,
        group_cols = input$group_filter
      )

      n_groups <- nrow(labor_movement_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_total(
          labor_movement_data,
          group = input$group_filter,
          x_col = "movement_cost",
          x_label = "Movement Costs"
        ),
        height = plot_height
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth in labor movement costs by group
    output$wagebill_movement_growth <- plotly::renderPlotly({
      validate(
        shiny::need(
          input$group_filter != "ref_date",
          "Please select a group."
        )
      )

      wagebill_movement_data <- govhr::compute_movement_cost(
        wagebill_filtered(),
        event_type = input$event_type,
        measure_col = input$wagebill_measure,
        group_cols = input$group_filter
      )

      # compute growth between min and max ref_date for each group
      wagebill_movement_growth_data <- wagebill_movement_data[
        ref_date %in% range(ref_date),
        .(
          growth_rate = (movement_cost[ref_date == max(ref_date)] -
            movement_cost[ref_date == min(ref_date)]) /
            movement_cost[ref_date == min(ref_date)]
        ),
        by = c(input$group_filter)
      ]

      n_groups <- nrow(wagebill_movement_growth_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_growth(
          wagebill_movement_growth_data,
          group = input$group_filter
        ),
        height = plot_height
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}

#' Wagebill retirement UI module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#' 
#' @import bslib
#' @import shiny
#' @importFrom plotly plotlyOutput
#' 
#' @return A Shiny module UI function for the wagebill retirement module.
wagebill_retirement_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = span("Controls", bsicons::bs_icon("sliders")),
      width = "300px",
      !!!ui_filter_controls(.data, id),
      shiny::numericInput(
        shiny::NS(id, "threshold_age"),
        label = "Select retirement threshold age:",
        value = 60,
        min = 50,
        max = 70
      ),
      shiny::selectInput(
        shiny::NS(id, "wagebill_measure"),
        "Type of Wage:",
        choices = identify_wagebill_choices(.data),
        selected = "gross_salary_lcu"
      ),
      shiny::actionButton(
        shiny::NS(id, "apply_btn"),
        "Apply selection",
        icon = shiny::icon("play")
      )
    ),
    # plot 1. retirement costs
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Retirement Costs",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Retirement costs over time. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_retirement"),
        height = "350px"
      )
    ),
    # plot 2. projected retirement costs
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Projected Retirement Costs",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Projected retirement costs over time. Choosing a group will add new trend lines, by group.",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "wagebill_retirement_projection"),
        height = "350px"
      )
    )
  )
}

#' Server for the wagebill retirement module.
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#' 
#' @import shiny
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom dplyr filter rename
#' 
#' @return A Shiny module server function for the wagebill retirement module.
wagebill_retirement_server <- function(id, .data) {
  shiny::moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    wagebill_filtered <- shiny::reactive({
      data <- .data

      if (input$group_filter != "ref_date") {
        data <- data |>
          dplyr::filter(
            .data[[input$group_filter]] %in% input$subgroup_filter
          )
      }

      data |>
        dplyr::filter(
          .data[["ref_date"]] >= input$date_range[1],
          .data[["ref_date"]] <= input$date_range[2]
        )
    })

    # plot 1. retirement costs
    output$wagebill_retirement <- plotly::renderPlotly({
      retirement_data <- govhr::compute_movement_cost(
        wagebill_filtered(),
        event_type = "retirement",
        measure_col = input$wagebill_measure,
        group_cols = input$group_filter
      )

      plotly::ggplotly(
        plot_trend(
          retirement_data,
          group = input$group_filter,
          y_col = "movement_cost",
          y_label = "Retirement Costs"
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. projected retirement costs
    output$wagebill_retirement_projection <- plotly::renderPlotly({
      retirement_projection_data <- govhr::project_retirement(
        wagebill_filtered(),
        threshold_age = input$threshold_age,
        group_cols = input$group_filter,
        measure_col = input$wagebill_measure
      ) |>
        rename(
          ref_date = retirement_date
        )

      plotly::ggplotly(
        plot_trend(
          retirement_projection_data,
          group = input$group_filter,
          y_col = "projected_cost",
          y_label = "Projected Retirement Costs"
        )
      )
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}

#' Function to render a wagebill summary value box.
#'
#' @param wagebill_data A data frame containing wagebill data.
#' @param type_measure A character string specifying which measure to display. Must be one of "total_wagebill" or "total_pension_liabilities".
#'
#' @import bslib
#' @importFrom shiny renderUI
#' @importFrom bsicons bs_icon
#' @importFrom scales comma
#' @importFrom dplyr filter pull
#'
#' @return A Shiny UI output rendering a value box summarizing the wage bill or pension liabilities total.
render_wagebill_box <- function(wagebill_data, type_measure) {
  measure_col <- "gross_salary_lcu"

  if (type_measure == "total_wagebill") {
    label <- "Total Wage Bill"

    total_value <- govhr::compute_fastsummary(
      wagebill_data |>
        dplyr::filter(.data[["ref_date"]] == max(.data[["ref_date"]]) & .data[["employment_status"]] == "active"),
      cols = measure_col,
      groups = "ref_date",
      fns = "sum"
    ) |>
      dplyr::pull(.data[["value"]])
  } else if (type_measure == "total_pension_liabilities") {
    label <- "Total Pension Liabilities"

    total_value <- govhr::compute_fastsummary(
      wagebill_data |>
        dplyr::filter(.data[["ref_date"]] == max(.data[["ref_date"]]) & .data[["employment_status"]] == "pensioner"),
      cols = measure_col,
      groups = "ref_date",
      fns = "sum"
    ) |>
      dplyr::pull(.data[["value"]])
  } else {
    stop("Invalid type_measure. Must be 'total_wagebill' or 'total_pension_liabilities'.")
  }

  shiny::renderUI({
    value_box(
      title = paste0(label, "(", max(wagebill_data[["ref_date"]]), ")"),
      value = scales::comma(total_value, accuracy = 1),
      showcase = switch(
        type_measure,
        "total_wagebill" = bsicons::bs_icon("currency-dollar"),
        "total_pension_liabilities" = bsicons::bs_icon("piggy-bank-fill")
      ),
      theme = value_box_theme(bg = "#C34729", fg = "#ffffff"),
      class = "border",
      max_height = "150px"
    )
  })
}