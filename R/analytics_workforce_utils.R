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

#' Plot a Movement Measure Over Time
#'
#' Draws one movement measure with [govhr::plot_trend()], for the measures
#' [govhr::plot_movement()] does not cover: replacement, retirement, projected
#' retirement and movement costs.
#'
#' @param data Data frame or lazy table (`tbl_dbi`) with `ref_date` and
#'   `y_col`. A lazy table is brought into memory first.
#' @param y_col Character. Column to plot.
#' @param y_label Character. y-axis label.
#' @param group_col Character. Column to draw one line per group, or
#'   `"ref_date"` (default) for a single line.
#' @param percent Logical. Format the y-axis as a percentage. Default `FALSE`.
#'
#' @return A ggplot2 object.
#'
#' @importFrom dplyr collect filter
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
  # would otherwise drop with a "removed rows" warning on every render.
  # plot_trend() sizes its group colours from `data[[group_col]]`, which is
  # NULL on a lazy table, hence the collect()
  plot <- data |>
    dplyr::filter(!is.na(.data[[y_col]])) |>
    dplyr::collect() |>
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
#' @param data A data frame or remote database table (`tbl_dbi`) containing
#'   personnel data with columns for ref_date, personnel_id, gender, educat7,
#'   employment_status, and birth_date.
#' @param movement_type A character string indicating the type of movement to profile (e.g
#' "hire" or "separation").
#'
#' @return A gt table summarizing the demographic characteristics of the specified movement type compared to the general population.
#' @importFrom dplyr all_of any_of coalesce collect distinct filter if_else
#'   left_join mutate select semi_join
#' @importFrom gtsummary tbl_summary modify_header as_gt
#' @importFrom stringr str_to_title
render_movement_profile <- function(data, movement_type) {
  profile_data <- data |>
    dplyr::select(
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

  movers <- detect_movement(profile_data) |>
    dplyr::select(personnel_id, ref_date, moved = dplyr::all_of(movement_type))

  # the first date has nothing to detect hires against, and the last nothing
  # to detect separations against, so records on it are left out
  comparable_dates <- movers |>
    dplyr::filter(!is.na(moved)) |>
    dplyr::distinct(ref_date)

  profile_data |>
    dplyr::semi_join(comparable_dates, by = "ref_date") |>
    dplyr::left_join(movers, by = c("personnel_id", "ref_date")) |>
    # pensioner records beside an active one share its flag, so only active
    # records are labelled as movers
    dplyr::mutate(
      type_event = dplyr::if_else(
        dplyr::coalesce(.data[["employment_status"]] == "active", FALSE) &
          dplyr::coalesce(moved, FALSE),
        !!movement_type,
        "stayed"
      )
    ) |>
    dplyr::select(-moved) |>
    # tbl_summary() summarises individual records, hence the collect()
    dplyr::collect() |>
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
