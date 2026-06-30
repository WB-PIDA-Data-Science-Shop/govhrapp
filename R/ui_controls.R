#' Identify available grouping choices based on the columns present in the data.
#' @param .data A data frame.
#' 
#' @import dplyr
#' @importFrom purrr set_names
#' @return A named list of grouping choices, where each element corresponds to a module and contains a named vector of variable IDs and their corresponding variable names.
identify_group_choices <- function(.data){
  available_cols <- names(.data)

  group_choices <- c(
      list("All" = "ref_date"),
      govhr::dictionary |>
        dplyr::filter(
          .data[["variable_id"]] %in%
            available_cols &
            .data[["variable_class"]] == "character" &
            !.data[["variable_id"]] %in%
              c("ref_date", "contract_id", "personnel_id")
        ) |>
        dplyr::group_by(.data[["module"]]) |>
        dplyr::summarise(
          choices = list(
            purrr::set_names(.data[["variable_id"]], .data[["variable_name"]])
          ),
          .groups = "drop"
        ) |>
        dplyr::pull(.data[["choices"]], name = .data[["module"]])
    )

  group_choices
}