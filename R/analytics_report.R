#' Generate Wage Bill Report
#'
#' Internal helper function to generate a Word document report with wage bill
#' visualizations and analysis.
#'
#' @param wagebill_summary_data Reactive expression containing summary data for plot 1
#' @param wagebill_filtered_data Reactive expression containing filtered date data
#' @param date_range Numeric vector of length 2 with start and end years
#' @param wagebill_measure Character string specifying wage measure column
#' @param wagebill_group Character string specifying grouping variable
#' @param toggle_growth Logical indicating whether to show growth rate view
#'
#' @return Character path to the generated Word document
#'
#' @keywords internal
#' @noRd
generate_wagebill_report <- function(wagebill_summary_data,
                                     wagebill_filtered_data,
                                     date_range,
                                     wagebill_measure,
                                     wagebill_group,
                                     toggle_growth) {
  
  # Convert plotly plots back to ggplot for static export
  plot1_static <- wagebill_summary_data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["ref_date"]],
        y = .data[["value"]]
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::xlab("Time") +
    {if (wagebill_group != "ref_date") 
      ggplot2::aes(group = .data[[wagebill_group]]) else NULL} +
    {if (toggle_growth) {
      list(
        ggplot2::ylab("Growth Rate (Base = 100%)"),
        ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "red3")
      )
    } else {
      ggplot2::ylab("Wage Bill")
    }}
  
  plot2_static <- if (wagebill_group != "ref_date") {
    wagebill_filtered_data |>
      dplyr::mutate(year = lubridate::year(.data[["ref_date"]])) |>
      dplyr::filter(year == max(year)) |>
      govhr::compute_fastsummary(
        cols = wagebill_measure,
        fns = "sum",
        groups = wagebill_group
      ) |>
      dplyr::filter(
        !is.na(.data[["value"]]) &
          !is.na(.data[[wagebill_group]])
      ) |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data[["value"]],
          y = stats::reorder(
            stringr::str_wrap(.data[[wagebill_group]], width = 30),
            .data[["value"]]
          )
        )
      ) +
      ggplot2::geom_col() +
      ggplot2::scale_x_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::labs(x = "Wage bill", y = "")
  } else NULL
  
  plot3_static <- if (wagebill_group != "ref_date") {
    wagebill_annual <- wagebill_filtered_data |>
      dplyr::mutate(year = lubridate::year(.data[["ref_date"]])) |>
      dplyr::filter(year %in% c(max(year), max(year) - 1)) |>
      govhr::compute_fastsummary(
        cols = wagebill_measure,
        fns = "sum",
        groups = c("ref_date", wagebill_group)
      )
    
    max_filtered_date <- max(wagebill_filtered_data$ref_date, na.rm = TRUE)
    
    wagebill_annual |>
      dplyr::group_by(dplyr::across(dplyr::all_of(wagebill_group))) |>
      govhr::complete_dates(
        id_col = wagebill_group,
        start_date = max_filtered_date - lubridate::years(1),
        end_date = max_filtered_date
      ) |>
      dplyr::mutate(
        growth_rate = round(.data[["value"]] / dplyr::lag(.data[["value"]]) - 1, 3) * 100
      ) |>
      dplyr::filter(
        .data[["ref_date"]] == max_filtered_date &
          !is.na(.data[["growth_rate"]]) &
          !is.na(.data[[wagebill_group]])
      ) |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data[["growth_rate"]],
          y = stats::reorder(
            stringr::str_wrap(.data[[wagebill_group]], width = 30),
            .data[["growth_rate"]]
          )
        )
      ) +
      ggplot2::geom_col() +
      ggplot2::geom_vline(xintercept = 0, linewidth = 1.25, linetype = "dashed", color = "#2958c3") +
      ggplot2::scale_x_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::labs(x = "Growth rate", y = "")
  } else NULL
  
  plot4_static <- if (wagebill_group != "ref_date") {
    wagebill_filtered_data |>
      govhr::convert_constant_ppp(
        cols = wagebill_measure,
        macro_indicators = govhr::macro_indicators
      ) |>
      dplyr::mutate(year = lubridate::year(.data[["ref_date"]])) |>
      dplyr::filter(year == max(year)) |>
      plot_segment(
        col = wagebill_measure,
        group = wagebill_group
      ) +
      ggplot2::labs(x = "Wage bill", y = "")
  } else NULL
  
  # Copy template to temp directory
  temp_report <- file.path(tempdir(), "wagebill_report.qmd")
  file.copy(
    system.file("markdown/wagebill_report.qmd", package = "govhrapp"),
    temp_report,
    overwrite = TRUE
  )
  
  # Generate output file path
  output_file <- file.path(
    tempdir(),
    paste0("wagebill_report_", format(Sys.Date(), "%Y%m%d"), ".docx")
  )
  
  # Render using rmarkdown with parameters
  rmarkdown::render(
    input = temp_report,
    output_file = output_file,
    params = list(
      date_range = date_range,
      wagebill_measure = wagebill_measure,
      wagebill_group = wagebill_group,
      plot1 = plot1_static,
      plot2 = plot2_static,
      plot3 = plot3_static,
      plot4 = plot4_static
    ),
    envir = new.env(),
    quiet = TRUE
  )
  
  return(output_file)
}

#' Generate Workforce Report
#'
#' Internal helper function to generate a Word document report with workforce
#' visualizations and analysis.
#'
#' @param workforce_summary_data Reactive expression containing summary data for plot 1
#' @param workforce_filtered_data Reactive expression containing filtered date data
#' @param date_range Numeric vector of length 2 with start and end years
#' @param workforce_group Character string specifying grouping variable
#' @param toggle_growth Logical indicating whether to show growth rate view
#' @param movement_type Character string specifying movement type: "hire", "fire", or "replacement_rate"
#'
#' @return Character path to the generated Word document
#'
#' @keywords internal
#' @noRd
generate_workforce_report <- function(workforce_summary_data,
                                      workforce_filtered_data,
                                      date_range,
                                      workforce_group,
                                      toggle_growth,
                                      movement_type = "hire") {
  
  # Convert plotly plots back to ggplot for static export
  plot1_static <- workforce_summary_data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["ref_date"]],
        y = .data[["value"]]
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("Time") +
    {if (workforce_group != "ref_date")
      ggplot2::aes(group = .data[[workforce_group]]) else NULL} +
    {if (toggle_growth) {
      list(
        ggplot2::scale_y_continuous(
          labels = scales::label_number(accuracy = 0.1)
        ),
        ggplot2::ylab("Baseline index (first period = 100)"),
        ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "red3")
      )
    } else {
      list(
        ggplot2::scale_y_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ),
        ggplot2::ylab("Headcount")
      )
    }}

  plot2_static <- if (workforce_group != "ref_date") {
    workforce_filtered_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(workforce_group))) |>
      dplyr::filter(year == max(year)) |>
      govhr::fastcount(.data[[workforce_group]], name = "value") |>
      dplyr::filter(
        !is.na(.data[["value"]]) &
          !is.na(.data[[workforce_group]])
      ) |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data[["value"]],
          y = stats::reorder(
            stringr::str_wrap(.data[[workforce_group]], width = 30),
            .data[["value"]]
          )
        )
      ) +
      ggplot2::geom_col() +
      ggplot2::scale_x_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::scale_y_discrete(
        guide = ggplot2::guide_axis(n.dodge = 2)
      ) +
      ggplot2::labs(x = "Headcount", y = "")
  } else NULL

  plot3_static <- if (workforce_group != "ref_date") {
    workforce_filtered_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(workforce_group))) |>
      dplyr::filter(
        .data[["ref_date"]] %in% c(max(.data[["ref_date"]]), min(.data[["ref_date"]]))
      ) |>
      govhr::fastcount(
        .data[["ref_date"]],
        .data[[workforce_group]],
        name = "value"
      ) |>
      dplyr::filter(!is.na(.data[[workforce_group]])) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(workforce_group))) |>
      dplyr::summarise(
        growth_rate = round(
          dplyr::last(.data[["value"]]) / dplyr::first(.data[["value"]]) - 1,
          3
        ) * 100,
        .groups = "drop"
      ) |>
      dplyr::filter(!is.na(.data[["growth_rate"]])) |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data[["growth_rate"]],
          y = stats::reorder(
            stringr::str_wrap(.data[[workforce_group]], width = 30),
            .data[["growth_rate"]]
          )
        )
      ) +
      ggplot2::geom_col() +
      ggplot2::geom_vline(xintercept = 0, linewidth = 1.25, linetype = "dashed", color = "#2958c3") +
      ggplot2::scale_x_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::scale_y_discrete(
        guide = ggplot2::guide_axis(n.dodge = 2)
      ) +
      ggplot2::labs(x = "Growth rate", y = "")
  } else NULL

  plot4_static <- {
    min_date <- min(workforce_filtered_data[["ref_date"]], na.rm = TRUE) |>
      as.character()
    max_date <- max(workforce_filtered_data[["ref_date"]], na.rm = TRUE) |>
      as.character()

    freq_ref_date <- workforce_filtered_data |>
      guess_date_frequency()

    group_cols <- unique(c("ref_date", workforce_group))

    if (movement_type %in% c("hire", "fire")) {
      movement_data <- workforce_filtered_data |>
        govhr::detect_personnel_event(
          event_type = movement_type,
          id_col = "personnel_id",
          start_date = min_date,
          end_date = max_date,
          freq = freq_ref_date
        ) |>
        dplyr::right_join(
          workforce_filtered_data,
          by = c("personnel_id", "ref_date")
        ) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
        dplyr::summarise(
          indicator = mean(!is.na(.data[["type_event"]])),
          .groups = "drop"
        )
    } else {
      hire_data <- workforce_filtered_data |>
        govhr::detect_personnel_event(
          event_type = "hire",
          id_col = "personnel_id",
          start_date = min_date,
          end_date = max_date,
          freq = freq_ref_date
        ) |>
        dplyr::left_join(
          workforce_filtered_data,
          by = c("personnel_id", "ref_date")
        ) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
        dplyr::summarise(hires = dplyr::n(), .groups = "drop")

      fire_data <- workforce_filtered_data |>
        govhr::detect_personnel_event(
          event_type = "fire",
          id_col = "personnel_id",
          start_date = min_date,
          end_date = max_date,
          freq = freq_ref_date
        ) |>
        dplyr::left_join(
          workforce_filtered_data,
          by = c("personnel_id", "ref_date")
        ) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
        dplyr::summarise(fires = dplyr::n(), .groups = "drop")

      movement_data <- hire_data |>
        dplyr::left_join(fire_data, by = group_cols) |>
        dplyr::mutate(indicator = .data[["hires"]] / .data[["fires"]])
    }

    plot_movement <- movement_data |>
      ggplot2::ggplot(
        ggplot2::aes(.data[["ref_date"]], .data[["indicator"]])
      ) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::labs(x = "Time", y = "Share")

    if (workforce_group != "ref_date") {
      n_groups <- dplyr::n_distinct(movement_data[[workforce_group]], na.rm = TRUE)
      orange_palette <- grDevices::colorRampPalette(c("#C34729", "#F5C6A0"))(n_groups)
      plot_movement <- plot_movement +
        ggplot2::aes(
          color = .data[[workforce_group]],
          group = .data[[workforce_group]]
        ) +
        ggplot2::scale_color_manual(values = orange_palette)
    }

    if (movement_type %in% c("hire", "fire")) {
      plot_movement <- plot_movement +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format()
        )
    } else {
      plot_movement <- plot_movement +
        ggplot2::scale_y_continuous(
          labels = scales::label_number(accuracy = 0.1)
        ) +
        ggplot2::geom_hline(
          yintercept = 1,
          linetype = "dashed",
          color = "#004181"
        ) +
        ggplot2::annotate(
          "text",
          x = as.Date(max_date) - (as.Date(max_date) - as.Date(min_date)) * 0.05,
          y = 1.15,
          label = "Replacement rate = 1",
          color = "#004181"
        ) +
        ggplot2::labs(y = "Replacement rate")
    }

    plot_movement
  }

  # Copy template to temp directory
  temp_report <- file.path(tempdir(), "workforce_report.qmd")
  file.copy(
    system.file("markdown/workforce_report.qmd", package = "govhrapp"),
    temp_report,
    overwrite = TRUE
  )
  
  # Generate output file path
  output_file <- file.path(
    tempdir(),
    paste0("workforce_report_", format(Sys.Date(), "%Y%m%d"), ".docx")
  )
  
  # Render using rmarkdown with parameters
  rmarkdown::render(
    input = temp_report,
    output_file = output_file,
    params = list(
      date_range = date_range,
      workforce_group = workforce_group,
      plot1 = plot1_static,
      plot2 = plot2_static,
      plot3 = plot3_static,
      plot4 = plot4_static
    ),
    envir = new.env(),
    quiet = TRUE
  )
  
  return(output_file)
}