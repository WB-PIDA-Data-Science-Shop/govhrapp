#' Compute coverage of non-missing values in a data frame or data table.
#' 
#' @param .data A data frame.
#' @param group_var A character string specifying the column name to group by. If NULL, coverage is computed for the entire data set.
#' @param aggregate A logical value indicating whether to aggregate coverage values by the `group_var`.
#' 
#' @import duckplyr
#' @import dplyr
#' @return A data frame with coverage values for each column, optionally grouped by the specified `group_var`.
compute_coverage <- function(.data, group_var = NULL, aggregate = FALSE) {
  if(!any(class(.data) %in% c("duckplyr_df", "tbl_duckdb_connection"))) {
    .data <- .data |> 
      duckplyr::as_duckplyr_tibble()
  }
  
  coverage_data <- .data |> 
      dplyr::group_by(dplyr::across(dplyr::any_of(group_var))) |>
      summarise(
       across(everything(), ~ sum(!is.na(.x))/n(), .names = "coverage_{col}"),
       .groups = "drop"
      )
  
  if(aggregate){
    coverage_data <- coverage_data |> 
      collect() |> 
      dplyr::group_by(dplyr::across(dplyr::any_of(group_var))) |>
      rowwise() |> 
      summarise(
        coverage = mean(c_across(starts_with("coverage_")), na.rm = TRUE),
        .groups = "drop"
      ) |> 
      duckplyr::as_duckplyr_tibble()
  }

  coverage_data
}