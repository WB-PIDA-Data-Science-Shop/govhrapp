#' Name the Column Holding a Movement Measure
#'
#' Maps a movement type and measurement type to the column that
#' [govhr::compute_movement()] or [compute_retirement()] stores it in.
#'
#' @param movement_type Character. One of `"hire"`, `"separation"`,
#'   `"retirement"` or `"replacement"`.
#' @param measurement_type Character. `"count"` or `"rate"`. Ignored for
#'   `"replacement"`, which is always a ratio.
#'
#' @return A column name, such as `"hires"`, `"separation_rate"` or
#'   `"replacement_rate"`.
#'
#' @keywords internal
movement_measure_col <- function(movement_type, measurement_type) {
  if (movement_type == "replacement" || measurement_type == "rate") {
    paste0(movement_type, "_rate")
  } else {
    paste0(movement_type, "s")
  }
}

#' Project Retirements and Their Rate
#'
#' Projects, for each future year, how many of the current active workforce
#' reach the retirement threshold age, and divides them by the current active
#' headcount.
#'
#' @param data Data frame containing personnel data with `personnel_id`,
#'   `ref_date`, `employment_status` and `birth_date`.
#' @param threshold_age Numeric. Age at which personnel retire. Default `60`.
#' @param group_cols Character vector of columns to group by, or `NULL`
#'   (default) for the whole workforce.
#'
#' @return A data frame with `ref_date` (the projected retirement date), the
#'   grouping columns, `projected_retirements`, `headcount` and
#'   `projected_retirement_rate`.
#'
#' @importFrom dplyr all_of cross_join filter left_join mutate n_distinct
#'   rename summarise
#' @importFrom govhr project_retirement
#' @keywords internal
compute_projected_retirement <- function(
  data,
  threshold_age = 60,
  group_cols = NULL
) {
  # govhr::project_retirement() projects everyone's last record, including
  # people who left before the latest date. restricting to the current active
  # workforce keeps the projected retirements inside the headcount they are
  # divided by
  current_workforce <- data |>
    dplyr::filter(
      .data[["ref_date"]] == max(.data[["ref_date"]], na.rm = TRUE),
      .data[["employment_status"]] == "active"
    )

  headcount <- current_workforce |>
    dplyr::summarise(
      headcount = dplyr::n_distinct(.data[["personnel_id"]]),
      .by = dplyr::all_of(group_cols)
    )

  projected <- current_workforce |>
    govhr::project_retirement(
      threshold_age = threshold_age,
      birth_col = "birth_date",
      group_cols = group_cols,
      simplify_retirement_date = TRUE
    ) |>
    dplyr::rename(
      ref_date = "retirement_date",
      projected_retirements = "indicator"
    )

  projected <- if (is.null(group_cols)) {
    dplyr::cross_join(projected, headcount)
  } else {
    dplyr::left_join(projected, headcount, by = group_cols)
  }

  projected |>
    dplyr::mutate(
      projected_retirement_rate = .data[["projected_retirements"]] /
        .data[["headcount"]]
    )
}

#' Plot a Movement Measure Over Time
#'
#' Draws one movement measure with [govhr::plot_trend()], for the measures
#' [govhr::plot_movement()] does not cover: replacement, retirement and
#' projected retirement.
#'
#' @param data Data frame with `ref_date` and `y_col`.
#' @param y_col Character. Column to plot.
#' @param y_label Character. y-axis label.
#' @param group_col Character. Column to draw one line per group, or
#'   `"ref_date"` (default) for a single line.
#' @param percent Logical. Format the y-axis as a percentage. Default `FALSE`.
#'
#' @return A ggplot2 object.
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom govhr plot_trend
#' @importFrom scales label_percent
#' @keywords internal
plot_movement_trend <- function(
  data,
  y_col,
  y_label,
  group_col = "ref_date",
  percent = FALSE
) {
  # movement measures are undefined at the first or last date, which ggplot2
  # would otherwise drop with a "removed rows" warning on every render
  plot <- data |>
    dplyr::filter(!is.na(.data[[y_col]])) |>
    govhr::plot_trend(group_col = group_col, y_col = y_col, y_label = y_label)

  if (!percent) {
    return(plot)
  }

  # replacing plot_trend()'s number axis is intended, so ggplot2's
  # scale-replacement message is noise
  suppressMessages(
    plot + ggplot2::scale_y_continuous(labels = scales::label_percent())
  )
}

#' Render a movement profile table
#'
#' @param data A data frame containing personnel data with columns for ref_date, personnel_id, gender, educat7, employment_status, and birth_date.
#' @param movement_type A character string indicating the type of movement to profile (e.g
#' "hire" or "separation").
#' 
#' @return A gt table summarizing the demographic characteristics of the specified movement type compared to the general population.
#' @importFrom dplyr select mutate
#' @importFrom gtsummary tbl_summary modify_header as_gt
#' @importFrom govhr classify_personnel_event guess_date_frequency
#' @importFrom stringr str_to_title
render_movement_profile <- function(data, movement_type) {
  movement_data <- data |>
    select(
      dplyr::any_of(
        c(
          "ref_date",
          "personnel_id",
          "gender",
          "educat7",
          "employment_status",
          "birth_date"
        )
      )
    )

  ref_dates <- movement_data[["ref_date"]]

  govhr::classify_personnel_event(
    data = movement_data,
    id_col = "personnel_id",
    # classify_personnel_event() predates govhr's hire/separation vocabulary
    event_type = if (movement_type == "separation") "fire" else movement_type,
    start_date = min(ref_dates),
    end_date = max(ref_dates),
    status_col = "employment_status",
    freq = govhr::guess_date_frequency(movement_data)
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
      include = c("age", "gender", "educat7", "employment_status"),
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
        stringr::str_to_title(movement_type)
      ),
      stat_2 = "**General Population**"
    ) |>
    gtsummary::as_gt()
}
