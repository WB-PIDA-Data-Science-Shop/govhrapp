#############################################################################################
########## SOME FUNCTIONS THAT CUT ACROSS MODULES AND INTERFACES OF THE QCHECK APP ##########
#############################################################################################

# (Helper functions now live in govhr package or in qcheck_internal_helpers.R)

#' a function to compute the proportion of missing values in a data frame
#' @param data A data frame.
#' @param digits An integer specifying the number of decimal places to round the result to. Default is 2.
#' 
#' @return A numeric value representing the proportion of missing values in the data frame.
compute_global_coverage <- function(data, digits = 2) {
  coverage <- 100 * mean(!is.na(data))

  coverage |>
    round(digits)
}

# compute_global_consistency <- function(data, digits = 2) {
#   record_consistency <- compute_record_consistency(data, digits)
#   value_consistency <- compute_value_consistency(data, digits)
    
#   global_consistency <- mean(c(record_consistency, value_consistency), na.rm = TRUE)

#   global_consistency |>
#     round(digits)
# }

#' Compute the proportion of consistent records in a data frame.
#' 
#' @param data A data frame.
#' @param id_col A character string specifying the name of the column that uniquely identifies records (e.g., "personnel_id" or "contract_id").
#' @param group_cols A character vector specifying the names of the columns to group by. Default is NULL, which means no grouping.
#' @param digits An integer specifying the number of decimal places to round the result to. Default is 2.
#' 
#' @import dplyr
#' @importFrom duckplyr as_duckplyr_tibble
#' 
#' @return A data frame with the proportion of consistent records in the data frame, optionally by group.
compute_record_consistency <- function(data, id_col, group_cols = NULL, digits = 2) {
  if (!any(class(data) %in% c("duckplyr_df", "tbl_duckdb_connection"))) {
    data <- data |>
      duckplyr::as_duckplyr_tibble()
  }

  group_cols_with_ref_date <- unique(
      c("ref_date", group_cols)
    )

  # compute the number of unique records based on the specified ID column
  record_consistency <- data |>
    dplyr::count(
      across(
        dplyr::all_of(c(id_col, group_cols_with_ref_date))
      )
    ) |>
    dplyr::mutate(
      consistent_record = if_else(.data[["n"]] == 1, 1, 0)
    )
  
  # percentage of consistent records, by group
  record_consistency |> 
    dplyr::summarise(
      record_consistency = round(100 * sum(consistent_record, na.rm = TRUE) / n(), digits),
      .by = dplyr::all_of(group_cols)
    )
}

#' Compute the proportion of consistent values in a data frame.
#' 
#' @param data A data frame.
#' @param id_col A character string specifying the name of the column that uniquely identifies records.
#' @param group_cols A character vector specifying the names of the columns to group by. Default is no grouping.
#' @param digits An integer specifying the number of decimal places to round the result to. Default is 2.
#' 
#' @import dplyr
#' @importFrom duckplyr as_duckplyr_tibble
#' 
#' @return A data frame with the proportion of consistent values in the data frame, optionally by group.
compute_value_consistency <- function(data, id_col, group_cols = NULL, digits = 2) {
  if (!any(class(data) %in% c("duckplyr_df", "tbl_duckdb_connection"))) {
    data <- data |>
      duckplyr::as_duckplyr_tibble()
  }

  group_cols_with_ref_date <- unique(
      c("ref_date", group_cols)
    )

  # compute the number of unique records based on the specified ID column
  value_consistency <- data |>
    dplyr::summarise(
      consistent_value = if_else(n_distinct(dplyr::across(-dplyr::all_of(c(id_col, group_cols_with_ref_date)))) == 1, 1, 0),
      .by = dplyr::all_of(
        c(id_col, group_cols_with_ref_date)
      )
    )
  
  # percentage of consistent values, by group
  value_consistency |> 
    dplyr::summarise(
      value_consistency = round(100 * sum(consistent_value, na.rm = TRUE) / n(), digits),
      .by = dplyr::all_of(group_cols)
    )
}

ui_filter_controls <- function(.data, id) {
  group_choices <- identify_group_choices(.data)

  # filter choices: optional groups from module structure, with a "None" entry on top
  filter_choices <- c(list("None" = "none"), group_choices[-1])

  list(
    shiny::sliderInput(
      shiny::NS(id, "date_range"),
      "Select date range:",
      min = min(.data[["ref_date"]], na.rm = TRUE),
      max = max(.data[["ref_date"]], na.rm = TRUE),
      value = c(
        min(.data[["ref_date"]], na.rm = TRUE),
        max(.data[["ref_date"]], na.rm = TRUE)
      ),
      timeFormat = "%Y-%m-%d"
    ),
    shiny::selectInput(
      shiny::NS(id, "group_filter"),
      "Select group:",
      choices = filter_choices
    ),
    shinyWidgets::pickerInput(
      shiny::NS(id, "subgroup_filter"),
      "Select subgroups:",
      choices = NULL,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        selectedTextFormat = "count > 3",
        countSelectedText = "{0} subgroups selected",
        noneSelectedText = "No subgroups selected",
        container = "body"
      )
    )
  )
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
      title = span("Filter", bsicons::bs_icon("filter")),
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

    # plot 1. coverage over time
    bslib::card(
      full_screen = TRUE,
      fillable = FALSE,
      bslib::card_header(
        "Coverage over time",
        bslib::tooltip(
          bsicons::bs_icon("info-circle"),
          "Coverage, by year. Choosing a group will add new coverages, by group."
        )
      ),
      layout_sidebar(
        sidebar = sidebar(
          title = "Group breakdown:",
          position = "right",
          shiny::selectInput(
            shiny::NS(id, "coverage_group"),
            "Group by",
            choices = identify_group_choices(.data),
            selected = "All"
          )
        ),
        plotly::plotlyOutput(
          shiny::NS(id, "coverage_panel"),
          height = "350px"
        )
      )
    ),

    # plot 2. coverage by variable
    bslib::card(
      full_screen = TRUE,
      fillable = FALSE,
      bslib::card_header(
        "Coverage by variable",
        bslib::tooltip(
          bsicons::bs_icon("info-circle"),
          "Coverage, by variable."
        )
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
        bslib::tooltip(
          bsicons::bs_icon("info-circle"),
          "Coverage, by variable and group."
        )
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
#'
#' @return A set of Shiny outputs for the coverage panel.
coverage_panel_server <- function(id, .data) {
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

    data_filtered <- shiny::reactive({
      data <- .data

      if (
        !is.null(input$group_filter) &&
          input$group_filter != "none" &&
          length(input$subgroup_filter) > 0
      ) {
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

    # plot 1. coverage over time
    output$coverage_panel <- plotly::renderPlotly({
      plot_coverage_trend(
        data_filtered(),
        group = input$coverage_group,
        toggle_growth = input$toggle_growth
      )
    })

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