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