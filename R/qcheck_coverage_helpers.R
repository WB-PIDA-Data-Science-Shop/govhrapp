#' Compute coverage of non-missing values in a dataset.
#'
#' @param .data A data frame.
#' @param group A character string specifying the column name to group by. If NULL, coverage is computed for the entire data set.
#' @param include_ref_date A logical value indicating whether to include the `ref_date` column in the grouping.
#' @param aggregate A logical value indicating whether to aggregate coverage values by the `group`.
#'
#' @import duckplyr
#' @import dplyr
#' @return A data frame with coverage values for each column, optionally grouped by the specified `group`.
compute_coverage <- function(.data, group = NULL, include_ref_date = FALSE, aggregate = FALSE) {
  if (!any(class(.data) %in% c("duckplyr_df", "tbl_duckdb_connection"))) {
    .data <- .data |>
      duckplyr::as_duckplyr_tibble()
  }

  if(include_ref_date) {
    group <- unique(
      c("ref_date", group)
    )
  }

  coverage_data <- .data |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        ~ (sum(!is.na(.x)) / dplyr::n()) * 100
      ),
      .by = dplyr::all_of(group)
    ) |>
    tidyr::pivot_longer(
        cols = -c(dplyr::all_of(group)),
        names_to = "variable",
        values_to = "coverage"
    ) 
    
  if (aggregate) {
    coverage_data <- coverage_data |>
      dplyr::summarise(
        coverage = mean(
          .data[["coverage"]],
          na.rm = TRUE
        ),
        .by = dplyr::all_of(group)
      )
  }

  coverage_data
}
