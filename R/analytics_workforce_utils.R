#' Name the column holding a movement measure
#'
#' Maps a movement type and measurement type to the column that
#' [govhr::compute_movement()] or [govhr::compute_retirement()] stores it in.
#'
#' @param movement_type Character. One of `"hire"`, `"separation"`,
#'   `"retirement"` or `"replacement"`.
#' @param measurement_type Character. `"count"` or `"rate"`. Ignored for
#'   `"replacement"`, which is always a ratio.
#'
#' @returns A column name, such as `"hires"`, `"separation_rate"` or
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

#' Plot a movement measure over time
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
#' @returns A ggplot2 object.
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
  # plot_trend() sizes its group colors from `data[[group_col]]`, which is
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
#' Compares the age, gender, education and employment status of hires or
#' separations with the rest of the workforce, pooling every reference date
#' the movement can be detected on.
#'
#' @param data Personnel data, as `workforce_data` in
#'   [run_govhrapp_analytics()].
#' @param movement_type Character. `"hire"` or `"separation"`.
#'
#' @returns A gt table with one column for the movers and one for everyone
#'   else.
#'
#' @details Age is measured on the day the table is rendered, not on each
#'   reference date.
#'
#' @importFrom dplyr all_of any_of coalesce collect distinct filter if_else
#'   left_join mutate select semi_join
#' @importFrom govhr detect_movement
#' @importFrom gtsummary tbl_summary modify_header as_gt
#' @keywords internal
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

  movers <- govhr::detect_movement(profile_data) |>
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
    # records are labeled as movers
    dplyr::mutate(
      type_event = dplyr::if_else(
        dplyr::coalesce(.data[["employment_status"]] == "active", FALSE) &
          dplyr::coalesce(moved, FALSE),
        !!movement_type,
        "stayed"
      )
    ) |>
    dplyr::select(-moved) |>
    # tbl_summary() summarizes individual records, hence the collect()
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
        "educat7" = "Education level",
        "employment_status" = "Employment status",
        "age" = "Age today"
      )
    ) |>
    gtsummary::modify_header(
      label = "**Characteristic**",
      stat_1 = sprintf("**%s**", movement_labels[[movement_type]]),
      stat_2 = "**Everyone else**"
    ) |>
    gtsummary::as_gt()
}
