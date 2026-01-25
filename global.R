library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)

workforce_data <- govhr::bra_hrmis_personnel |>
  mutate(
    educat7 = as.numeric(.data[["educat7"]]) |> 
      recode(
        `1` = "No formal education",
        `2` = "Some primary education",
        `3` = "Completed primary education",
        `4` = "Some secondary education",
        `5` = "Completed secondary education",
        `6` = "Some tertiary education",
        `7` = "Completed tertiary education"
      )
  ) |>
  mutate(
    educat7 = factor(
      educat7,
      levels = c(
        "No formal education",
        "Some primary education",
        "Completed primary education",
        "Some secondary education",
        "Completed secondary education",
        "Some tertiary education",
        "Completed tertiary education"
      ),
      ordered = TRUE
    )
  ) |>
  dplyr::filter(lubridate::year(.data[["ref_date"]]) <= 2017) |>
  distinct(.data[["ref_date"]], .data[["personnel_id"]], .keep_all = TRUE) |>
  select(all_of(c("ref_date", "personnel_id", "gender", "educat7", "status")))

wagebill_data <- govhr::bra_hrmis_contract |>
  dplyr::filter(lubridate::year(.data[["ref_date"]]) <= 2017) |>
  dplyr::left_join(
    govhr::bra_hrmis_personnel |>
      distinct(
        .data[["ref_date"]],
        .data[["personnel_id"]],
        .keep_all = TRUE
      ) |>
      select(all_of(c(
        "ref_date",
        "personnel_id",
        "gender",
        "educat7",
        "status"
      ))),
    by = c("ref_date", "personnel_id")
  )
