workforce_overview_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = span("Controls", bsicons::bs_icon("sliders")),
      width = "300px",
      !!!ui_filter_controls(.data, id),
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
        "Headcount",
        bslib::tooltip(
          bsicons::bs_icon("info-circle"),
          "Headcount trends over time. Choosing a group will add new trend lines, by group."
        )
      ),
      plotly::plotlyOutput(
        shiny::NS(id, "workforce_panel"),
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
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Headcount total, by group. Total refers to the latest available year in the selected time frame."
          )
        ),
        plotly::plotlyOutput(shiny::NS(id, "workforce_cross_section")),
        min_height = "450px"
      ),
      bslib::card(
        full_screen = TRUE,
        fillable = FALSE,
        bslib::card_header(
          "Growth rate by group",
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Growth rate with respect to first reference date, by group."
          )
        ),
        plotly::plotlyOutput(shiny::NS(id, "workforce_growth")),
        min_height = "450px"
      )
    )
  )
}

#' Workforce Movement UI
#'
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing personnel data.
#' @param type_movement A character string specifying the type of movement: "hire" or "fire".
#'
#' @import shiny
#' @import bslib
#' @importFrom plotly plotlyOutput
#' @importFrom gt gt_output
#'
#' @return A Shiny UI function for the workforce movement module.
workforce_movement_ui <- function(
  id,
  .data,
  type_movement = c("hire", "fire", "turnover", "retirement")
) {
  # move profile table right under movements over time
  # organize the outputs into two buckets: high-level and deep-dive
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = span("Controls", bsicons::bs_icon("sliders")),
      width = "300px",
      !!!ui_filter_controls(.data, id),
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
        sprintf("%ss over time", stringr::str_to_title(type_movement)),
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          sprintf(
            "The number of new %ss and rate (new %ss / total workforce) over time. The rate is computed as the number of new %ss divided by the total workforce at the beginning of each period.",
            type_movement,
            type_movement,
            type_movement
          ),
          title = sprintf("New %ss over time", type_movement),
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, sprintf("%s_plot", type_movement)))
    ),

    # table 1. demographic characteristics of movers vs. general pop.
    if (type_movement %in% c("hire", "fire")) {
      bslib::card(
        bslib::card_header(
          sprintf("Profile of new %ss", type_movement),
          bslib::popover(
            bsicons::bs_icon("info-circle-fill"),
            sprintf(
              "Compare the characteristics of new %ss against the general population, selecting which attributes to compare them with.",
              type_movement
            ),
            title = sprintf("Profile of new %ss", type_movement),
            placement = "left"
          )
        ),
        gt::gt_output(shiny::NS(id, sprintf("%s_profile", type_movement)))
      )
    },

    # plot 2. counts and rates by group
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        sprintf("%ss by group", stringr::str_to_title(type_movement)),
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          sprintf(
            "The number of new %ss and rate (new %ss / total workforce) by group. The rate is computed as the number of new %ss divided by the total workforce at the beginning of each period.",
            type_movement,
            type_movement,
            type_movement
          ),
          title = sprintf("%ss by group", stringr::str_to_title(type_movement)),
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(
        id,
        sprintf("%s_cross_section", type_movement)
      ))
    ),

    # plot 3. growth rate of counts and rates by group
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        sprintf("%ss growth by group", stringr::str_to_title(type_movement)),
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          sprintf(
            "The growth rate of new %ss and rate (new %ss / total workforce) by group. The growth rate is computed as the percentage change in the number of new %ss divided by the total workforce at the beginning of each period.",
            type_movement,
            type_movement,
            type_movement
          ),
          title = sprintf(
            "%ss growth by group",
            stringr::str_to_title(type_movement)
          ),
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, sprintf("%s_growth", type_movement)))
    )
  )
}

#' Workforce Overview Server
#'
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing workforce data.
#'
#' @import shiny
#' @import bslib
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom shinyWidgets updatePickerInput
#'
#' @return A Shiny server function for the workforce overview module.
workforce_overview_server <- function(id, .data) {
  shiny::moduleServer(id, function(input, output, session) {
    # update subgroup_filter choices whenever the group column changes
    shiny::observe({
      variable <- input$group_filter

      if (is.null(variable) || variable == "none") {
        shinyWidgets::updatePickerInput(
          session,
          "subgroup_filter",
          choices = NULL,
          selected = character(0)
        )
      } else {
        filter_vals <- sort(
          as.character(
            unique(
              stats::na.omit(.data[[variable]])
            )
          )
        )

        shinyWidgets::updatePickerInput(
          session,
          "subgroup_filter",
          choices = filter_vals,
          selected = filter_vals
        )
      }
    })

    workforce_filtered <- shiny::reactive({
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

    workforce_summary <- reactive({
      out <- compute_trend_summary(
        workforce_filtered(),
        group = input$group_filter
      )

      if (input$toggle_growth) {
        out <- apply_baseline_index(out, group = input$group_filter)
      }

      out
    })

    # plot 1. panel
    output$workforce_panel <- plotly::renderPlotly({
      plotly::ggplotly(
        plot_trend(
          workforce_summary(),
          group = input$group_filter,
          toggle_growth = input$toggle_growth,
          y_label = "Headcount"
        )
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. total by group
    output$workforce_cross_section <- plotly::renderPlotly({
      validate(
        need(input$group_filter != "ref_date", "Please select a group.")
      )

      cross_section_data <- compute_cross_section_summary(
        workforce_filtered(),
        group = input$group_filter
      )

      n_groups <- nrow(cross_section_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_total(
          cross_section_data,
          group = input$group_filter,
          x_label = "Headcount"
        ),
        height = plot_height
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth rate by group
    output$workforce_growth <- plotly::renderPlotly({
      validate(
        need(input$group_filter != "ref_date", "Please select a group.")
      )

      change_data <- compute_growth_summary(
        workforce_filtered(),
        group = input$group_filter
      )

      n_groups <- nrow(change_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_growth(change_data, group = input$group_filter),
        height = plot_height
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}

#' Workforce Movement Server
#'
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing personnel data.
#' @param movement_type A character string specifying the type of movement: "hire", "fire", or "turnover".
#'
#' @import shiny
#' @import bslib
#' @importFrom plotly renderPlotly
#' @importFrom gt render_gt
#' @importFrom gtsummary tbl_summary modify_spanning_header modify_header as_gt
#' @importFrom dplyr filter
#'
#' @return A Shiny server function for the workforce movement module.
workforce_movement_server <- function(
  id,
  .data,
  movement_type = c("hire", "fire", "turnover")
) {
  shiny::moduleServer(id, function(input, output, session) {
    # update subgroup_filter choices whenever the group column changes
    shiny::observe({
      variable <- input$group_filter

      if (is.null(variable) || variable == "none") {
        shinyWidgets::updatePickerInput(
          session,
          "subgroup_filter",
          choices = NULL,
          selected = character(0)
        )
      } else {
        filter_vals <- sort(
          as.character(
            unique(
              stats::na.omit(.data[[variable]])
            )
          )
        )

        shinyWidgets::updatePickerInput(
          session,
          "subgroup_filter",
          choices = filter_vals,
          selected = filter_vals
        )
      }
    })

    movement_type <- match.arg(
      movement_type,
      choices = c("hire", "fire", "turnover")
    )

    data_filtered <- shiny::reactive({
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

    # plot 1. hiring counts/rates over time
    output[[sprintf("%s_plot", movement_type)]] <- plotly::renderPlotly({
      plot_data <- generate_movement_data(
        .data = data_filtered(),
        movement_type = movement_type,
        measurement_type = input$measurement_type,
        group_cols = input$group_filter
      )

      plot_movement(
        plot_data,
        movement_type = movement_type,
        measurement_type = input$measurement_type,
        group_cols = input$group_filter
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. counts/rates by group
    output[[sprintf(
      "%s_cross_section",
      movement_type
    )]] <- plotly::renderPlotly({
      validate(
        need(input$group_filter != "ref_date", "Please select a group.")
      )

      cross_section_data <- generate_movement_data(
        .data = data_filtered(),
        movement_type = movement_type,
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
          x_label = stringr::str_to_title(movement_type)
        ),
        height = plot_height
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth rate by group
    output[[sprintf("%s_growth", movement_type)]] <- plotly::renderPlotly({
      validate(
        need(input$group_filter != "ref_date", "Please select a group.")
      )

      turnover_data <- generate_movement_data(
        .data = data_filtered(),
        movement_type = movement_type,
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
    if (movement_type %in% c("hire", "fire")) {
      output[[sprintf("%s_profile", movement_type)]] <- gt::render_gt({
        profile_data <- classify_personnel_event(
          .data = data_filtered(),
          id_col = "personnel_id",
          event_type = movement_type,
          start_date = min(data_filtered()[["ref_date"]]),
          end_date = max(data_filtered()[["ref_date"]]),
          status_col = "employment_status",
          freq = guess_date_frequency(data_filtered())
        ) |>
          # modify personnel data to include age and exclude birth_date
          dplyr::mutate(
            age = as.numeric(difftime(Sys.Date(), birth_date, units = "days")) /
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
          # label hires and fires as "New Hires" and "New Fires" and
          # stayed as "General Population"
          gtsummary::modify_header(
            update = list(
              label = "**Variable**",
              stat_1 = sprintf("**New %ss**", movement_type),
              stat_2 = "**General Population**"
            )
          ) |>
          gtsummary::as_gt()
      }) |>
        bindEvent(input$apply_btn, ignoreNULL = FALSE)
    }
  })
}

#' Workforce Retirement UI
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing personnel data.
#' 
#' @import shiny
#' @import bslib
#' @importFrom plotly plotlyOutput
#' 
#' @return A Shiny UI function for the workforce retirement module.
workforce_retirement_ui <- function(
  id,
  .data
) {
  bslib::layout_sidebar(
    fillable = FALSE,
    theme = bslib::bs_theme(bootswatch = "litera"),
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

    # plot 1. retirement counts/rates over time
    bslib::card(
      bslib::card_header(
        "Retirements over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "The number of retirements and rate (retirements / total workforce) over time. The rate is computed as the number of retirements divided by the total workforce at the beginning of each period.",
          title = "Retirements over time",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "retirement_plot"))
    ),

    # plot 2. projected retirements
    bslib::card(
      bslib::card_header(
        "Projected retirements",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "The projected number of retirements and rate (projected retirements / total workforce) based on the selected retirement threshold age.",
          title = "Projected retirements",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "retirement_expected_plot"))
    )
  )
}

#' Workforce Retirement Server
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing personnel data.
#' 
#' @import shiny
#' @import bslib
#' @importFrom plotly renderPlotly
#' @importFrom dplyr filter rename
#' 
#' @return A Shiny server function for the workforce retirement module.
workforce_retirement_server <- function(
  id,
  .data
) {
  shiny::moduleServer(id, function(input, output, session) {
    data_filtered <- shiny::reactive({
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

    # plot 1. retirement counts/rates over time
    output[["retirement_plot"]] <- plotly::renderPlotly({
      plot_data <- generate_movement_data(
        .data = data_filtered(),
        movement_type = "retirement",
        measurement_type = input$measurement_type,
        group_cols = input$group_filter
      )

      plot_movement(
        plot_data,
        movement_type = "retirement",
        measurement_type = input$measurement_type,
        group_cols = input$group_filter
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. projected retirements
    output[["retirement_expected_plot"]] <- plotly::renderPlotly({
      plot_data <- project_retirement(
        workforce_data = data_filtered(),
        threshold_age = input$threshold_age,
        birth_col = "birth_date",
        group_cols = input$group_filter,
        simplify_retirement_date = TRUE
      ) |>
        rename(ref_date = "retirement_date")

      plot_movement(
        plot_data,
        movement_type = "retirement",
        measurement_type = input$measurement_type,
        group_cols = input$group_filter
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}
