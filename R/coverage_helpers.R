#' Compute coverage of non-missing values in a dataset.
#'
#' @param .data A data frame.
#' @param group_var A character string specifying the column name to group by. If NULL, coverage is computed for the entire data set.
#' @param aggregate A logical value indicating whether to aggregate coverage values by the `group_var`.
#'
#' @import duckplyr
#' @import dplyr
#' @return A data frame with coverage values for each column, optionally grouped by the specified `group_var`.
compute_coverage <- function(.data, group_var = NULL, aggregate = FALSE) {
  if (!any(class(.data) %in% c("duckplyr_df", "tbl_duckdb_connection"))) {
    .data <- .data |>
      duckplyr::as_duckplyr_tibble()
  }

  coverage_data <- .data |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_var))
    ) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        ~ sum(!is.na(.x)) / dplyr::n()
      ),
      .groups = "drop"
    ) |> 
    tidyr::pivot_longer(
        cols = -c(dplyr::all_of(group_var)),
        names_to = "variable",
        values_to = "coverage"
    ) 
    
  if (aggregate) {
    coverage_data <- coverage_data |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(group_var))
      ) |>
      dplyr::summarise(
        coverage = mean(
          coverage,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  }

  coverage_data
}
