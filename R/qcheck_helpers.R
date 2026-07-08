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

#' Compute the proportion of consistent records and values in a data frame.
#'
#' @param data A data frame.
#' @param id_col A character string specifying the name of the column that uniquely identifies records.
#' @param value_cols A character vector specifying the name(s) of columns whose values
#'   are to be checked for consistency. Value consistency is computed separately for
#'   each column and averaged across columns before being combined with record consistency.
#' @param digits An integer specifying the number of decimal places to round the result to. Default is 2.
#' 
#' @import dplyr
#' @importFrom duckplyr as_duckplyr_tibble
#' @importFrom purrr map_dbl
#'
#' @return A numeric value representing the proportion of consistent records and values in the data frame.
#' @details Consistency is defined as the proportion of records and values that are consistent
#'   across the dataset. A record is considered consistent if it has a unique identifier and all
#'   its associated values are consistent. A value is considered consistent if it does not
#'   contradict other values for the same record.
compute_global_consistency <- function(data, id_col, value_cols, digits = 2) {
  # don't round intermediate results, to avoid compounding rounding error in the average
  record_consistency <- compute_record_consistency(
    data,
    id_col,
    digits = 10
  ) |>
    dplyr::pull(.data[["record_consistency"]])

  value_consistency <- purrr::map_dbl(
    value_cols,
    \(value_col) {
      compute_value_consistency(
        data,
        id_col,
        value_col,
        digits = 10
      ) |>
        dplyr::pull(.data[["value_consistency"]])
    }
  ) |>
    mean(na.rm = TRUE)

  global_consistency <- mean(
    c(record_consistency, value_consistency),
    na.rm = TRUE
  )

  global_consistency |>
    round(digits)
}

#' Render a Data Coverage Value Box
#'
#' @param .data Data frame. A dataframe to compute the coverage on.
#' @param title String. Value box title.
#' @param icon String. Icon name. Defaults to `"table"`.
#'
#' @return A [shiny::renderUI()] producing a [bslib::value_box()] themed
#'   `"danger"` (<50%), `"warning"` (50–79%), or `"success"` (≥80%).
#'
#' @importFrom shiny renderUI
#' @importFrom bslib value_box
#' @importFrom dplyr case_when
#'
#' @details Coverage denotes the total number of non-missing records in a dataset, expressed as a percentage of the total records. For example, if a dataset has 10 rows and 10 columns, i.e., 100 records, and 10 of them are available, the coverage would be 10 percent.
#' @export
render_coverage_box <- function(.data, title, icon = "table") {
  shiny::renderUI({
    value_coverage <- compute_global_coverage(.data)

    theme <- dplyr::case_when(
      value_coverage < 50                         ~ "danger",
      value_coverage >= 50 & value_coverage < 80 ~ "warning",
      TRUE                                        ~ "success"
    )

    bslib::value_box(
      title = title,
      value = value_coverage,
      icon  = icon,
      theme = theme
    )
  })
}

#' Render a Data Consistency Value Box
#'
#' @param .data Data frame. A dataframe to compute the consistency on.
#' @param title String. Value box title.
#' @param id_col String. The name of the column that uniquely identifies records (e.g., "personnel_id" or "contract_id").
#' @param value_cols Character vector. The names of the columns whose values are to be checked for consistency.
#' @param icon String. Icon name. Defaults to `"table"`.
#'
#' @return A [shiny::renderUI()] producing a [bslib::value_box()] themed
#'   `"danger"` (<50%), `"warning"` (50–79%), or `"success"` (≥80%).
#'
#' @importFrom shiny renderUI
#' @importFrom bslib value_box
#' @importFrom dplyr case_when
#'
#' @details Consistency denotes the proportion of records and values that are consistent across the dataset. A record is considered consistent if it has a unique identifier and all its associated values are consistent. A value is considered consistent if it does not contradict other values for the same record.
#' @export
render_consistency_box <- function(.data, id_col, value_cols, title, icon = "table") {
  shiny::renderUI({
    consistency <- compute_global_consistency(.data, id_col, value_cols)

    consistency_record <- compute_record_consistency(.data, id_col) |>
      dplyr::pull(.data[["record_consistency"]])
    
    consistency_value <- purrr::map_dbl(
      value_cols,
      \(value_col) {
        compute_value_consistency(.data, id_col, value_col) |>
          dplyr::pull(.data[["value_consistency"]])
      }
    ) |>
      mean(na.rm = TRUE)

    theme <- dplyr::case_when(
      consistency < 50                         ~ "danger",
      consistency >= 50 & consistency < 80 ~ "warning",
      TRUE                                        ~ "success"
    )

    bslib::value_box(
      title = title,
      value = paste0(consistency, "%"),
      icon  = icon,
      theme = theme,
      p(paste0("Record consistency: ", consistency_record, "%")),
      p(paste0("Value consistency: ", consistency_value, "%"))
    )
  })
}

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
compute_record_consistency <- function(
  data,
  id_col,
  group_cols = NULL,
  digits = 2
) {
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
      record_consistency = round(
        100 * sum(.data[["consistent_record"]], na.rm = TRUE) / n(),
        digits
      ),
      .by = dplyr::all_of(group_cols)
    )
}

#' Compute the proportion of consistent values in a data frame.
#'
#' @param data A data frame.
#' @param id_col A character string specifying the name of the column that uniquely identifies records.
#' @param value_col A character string specifying the name of the column whose values are to be checked for consistency.
#' @param group_cols A character vector specifying the names of the columns to group by. Default is no grouping.
#' @param digits An integer specifying the number of decimal places to round the result to. Default is 2.
#'
#' @import dplyr
#' @importFrom duckplyr as_duckplyr_tibble
#'
#' @return A data frame with the proportion of consistent values in the data frame, optionally by group.
compute_value_consistency <- function(
  data,
  id_col,
  value_col,
  group_cols = NULL,
  digits = 2
) {
  if (!any(class(data) %in% c("duckplyr_df", "tbl_duckdb_connection"))) {
    data <- data |>
      duckplyr::as_duckplyr_tibble()
  }

  # compute the number of unique values based on the specified ID column and grouping
  value_consistency <- data |>
    dplyr::summarise(
      consistent_value = if_else(
        n_distinct(
          .data[[value_col]]
        ) == 1,
        1,
        0
      ),
      .by = dplyr::all_of(
        c(id_col, group_cols)
      )
    )

  # percentage of consistent values, by group
  value_consistency |>
    dplyr::summarise(
      value_consistency = round(
        100 * sum(.data[["consistent_value"]], na.rm = TRUE) / n(),
        digits
      ),
      .by = dplyr::all_of(group_cols)
    )
}

ui_filter_controls <- function(.data, id) {
  group_choices <- identify_group_choices(.data)

  # filter choices: optional groups from module structure, with a "None" entry on top
  filter_choices <- c(list("None" = "none"), group_choices[-1])

  list(
    shiny::dateRangeInput(
      shiny::NS(id, "date_range"),
      "Select date range:",
      start = min(.data[["ref_date"]], na.rm = TRUE),
      end = max(.data[["ref_date"]], na.rm = TRUE),
      min = min(.data[["ref_date"]], na.rm = TRUE),
      max = max(.data[["ref_date"]], na.rm = TRUE)
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

consistency_panel_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = span("Filter", bsicons::bs_icon("filter")),
      width = "300px",
      !!!ui_filter_controls(.data, id),
      shiny::selectInput(
        shiny::NS(id, "type_plot"),
        "Select plot type:",
        choices = c(
          "Record" = "record",
          "Value"  = "value"
        ),
        selected = "record"
      ),
      # conditionally show the value column selection only when "Value" plot type is selected
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] === 'value'", shiny::NS(id, "type_plot")),
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
        bslib::tooltip(
          bsicons::bs_icon("info-circle"),
          "Consistency, by year. Choosing a group will add new consistencies, by group."
        )
      ),
      layout_sidebar(
        sidebar = sidebar(
          title = "Group breakdown:",
          position = "right",
          shiny::selectInput(
            shiny::NS(id, "consistency_group"),
            "Group by",
            choices = identify_group_choices(.data),
            selected = "All"
          )
        ),
        plotly::plotlyOutput(
          shiny::NS(id, "consistency_panel"),
          height = "350px"
        )
      )
    )
  )
}

consistency_panel_server <- function(id, .data) {
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

    # plot 1. consistency over time
    output$consistency_panel <- plotly::renderPlotly({
      id_col <- switch(
        id,
        "est" = "est_id",
        "personnel" = "personnel_id",
        "contract" = "contract_id"
      )

      plot_consistency_trend(
        data_filtered(),
        id_col = id_col,
        type_plot = input$type_plot,
        group = input$consistency_group,
        value_col = input$value_col,
        toggle_growth = input$toggle_growth
      )
    }) |>
        shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

  #   # plot 2. consistency by variable
  #   output$consistency_by_variable <- plotly::renderPlotly({
  #     plot_consistency_bar(data_filtered())
  #   }) |>
  #     shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

  #   # plot 3. consistency heatmap by group
  #   output$consistency_heatmap <- plotly::renderPlotly({
  #     plot_consistency_heatmap(
  #       data_filtered(),
  #       group = input$group_filter
  #     )
  #   }) |>
  #     shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}