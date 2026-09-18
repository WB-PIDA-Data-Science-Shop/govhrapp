#' Render a movement profile table
#' 
#' @param data A data frame containing personnel data with columns for ref_date, personnel_id, gender, educat7, employment_status, and birth_date.
#' @param movement_type A character string indicating the type of movement to profile (e.g
#' "hire" or "fire").
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
    event_type = movement_type,
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
