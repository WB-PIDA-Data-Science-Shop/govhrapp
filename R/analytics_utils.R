# shared by the value boxes, charts and profile table, so each movement is
# named the same way wherever it appears
movement_labels <- c(
  hire = "Hires",
  separation = "Separations",
  retirement = "Retirements",
  replacement = "Replacement rate"
)

#' Format a movement rate for display
#'
#' @param rate Numeric. A rate as a proportion, or the replacement ratio.
#' @param movement_type Character. One of `"hire"`, `"separation"`,
#'   `"retirement"` or `"replacement"`.
#'
#' @returns Character. A percentage such as `"3.1%"`, or for `"replacement"` a
#'   ratio such as `"1.49"`, or `"No separations"` when there were hires but
#'   no separations.
#'
#' @importFrom scales label_number label_percent
#' @keywords internal
format_movement_rate <- function(rate, movement_type) {
  # retirements are not separations, so a period with hires and no other
  # exits is common enough in small groups that "Inf" would otherwise show
  if (movement_type == "replacement" && is.infinite(rate)) {
    return("No separations")
  }

  # the replacement rate compares hires with separations rather than with
  # headcount, so a percentage would misread a ratio of 1.49 as 149% of staff
  if (movement_type == "replacement") {
    scales::label_number(accuracy = 0.01)(rate)
  } else {
    scales::label_percent(accuracy = 0.1)(rate)
  }
}

#' Render a workforce movement value box
#'
#' Renders the count and rate of one movement type at the latest reference
#' date. Values are read from the analytics cache when available and computed
#' from `.data` otherwise.
#'
#' @param .data Personnel data, as `workforce_data` in
#'   [run_govhrapp_analytics()].
#' @param movement_type Character. One of `"hire"`, `"separation"`,
#'   `"retirement"` or `"replacement"`.
#' @param cache List of precomputed summaries from [build_analytics_cache()],
#'   or `NULL` (default) to compute the values from `.data`.
#'
#' @returns A Shiny render function producing the value box.
#'
#' @importFrom bsicons bs_icon
#' @importFrom bslib popover value_box value_box_theme
#' @importFrom govhr compute_movement compute_retirement
#' @importFrom purrr pluck
#' @importFrom shiny h5 renderUI tagList
#' @keywords internal
render_movement_box <- function(.data, movement_type, cache = NULL) {
  values <- purrr::pluck(cache, "workforce", "movement_box", movement_type)

  if (is.null(values)) {
    values <- summarize_movement_box(
      govhr::compute_movement(.data),
      govhr::compute_retirement(.data),
      movement_type
    )
  }

  rate <- format_movement_rate(values[["rate"]], movement_type)

  box_value <- if (movement_type == "replacement" && is.infinite(values[["rate"]])) {
    shiny::h5("Hires but no separations")
  } else if (movement_type == "replacement") {
    shiny::h5(paste(rate, "hires per separation"))
  } else {
    shiny::tagList(
      shiny::h5(paste("Count:", values[["count"]])),
      shiny::h5(paste("Rate:", rate))
    )
  }

  shiny::renderUI({
    bslib::value_box(
      title = movement_labels[[movement_type]],
      theme = bslib::value_box_theme(bg = "#C34729", fg = "#ffffff"),
      class = "border",
      value = box_value,
      showcase = bsicons::bs_icon(
        switch(
          movement_type,
          hire = "person-plus-fill",
          separation = "person-dash-fill",
          retirement = "person-badge-fill",
          replacement = "arrow-repeat"
        )
      ),
      p(
        paste0(
          "Reference date: ",
          format(as.Date(values[["ref_date"]]), "%b %Y")
        ),
      ),
      bslib::popover(
        bsicons::bs_icon("info-circle-fill"),
        placement = "left",
        switch(
          movement_type,
          hire = "Personnel active on the reference date but not on the previous one, as a count and as a share of the previous reference date's active headcount.",
          separation = "Personnel active on the previous reference date but not on this one, other than retirees, as a count and as a share of the previous reference date's active headcount.",
          retirement = "Personnel active on the previous reference date who are recorded as pensioners on this one, as a count and as a share of the previous reference date's active headcount.",
          replacement = "Hires per separation. Retirements are not counted as separations. Above 1, hires outnumber separations."
        )
      )
    )
  })
}
