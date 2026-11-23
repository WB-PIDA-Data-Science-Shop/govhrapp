#' Format a Movement Rate for Display
#'
#' @param rate Numeric. A rate as a proportion, or the replacement ratio.
#' @param movement_type Character. One of `"hire"`, `"separation"`,
#'   `"retirement"` or `"replacement"`.
#'
#' @return Character. A percentage such as `"3.1%"`, or for `"replacement"` a
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

#' Render a Workforce Movement Value Box
#'
#' Renders the count and rate of one movement type at the latest reference
#' period. Values are read from the analytics cache when available and computed
#' from `.data` otherwise.
#'
#' @param .data Data frame containing personnel data.
#' @param movement_type Character. One of `"hire"`, `"separation"`,
#'   `"retirement"` or `"replacement"`.
#' @param cache List of pre-computed summaries from [build_analytics_cache()],
#'   or `NULL` to compute the values directly.
#'
#' @return A Shiny render function producing the value box.
#'
#' @importFrom bsicons bs_icon
#' @importFrom bslib popover value_box value_box_theme
#' @importFrom govhr compute_movement compute_retirement
#' @importFrom purrr pluck
#' @importFrom shiny h5 renderUI tagList
#' @importFrom stringr str_to_title
#' @keywords internal
render_movement_box <- function(.data, movement_type, cache = NULL) {
  values <- purrr::pluck(cache, "workforce", "movement_box", movement_type)

  if (is.null(values)) {
    values <- summarise_movement_box(
      govhr::compute_movement(.data),
      govhr::compute_retirement(.data),
      movement_type
    )
  }

  rate <- format_movement_rate(values[["rate"]], movement_type)

  box_value <- if (movement_type == "replacement" && is.infinite(values[["rate"]])) {
    shiny::tagList(
      shiny::h5("Hires with"),
      shiny::h5("no separations")
    )
  } else if (movement_type == "replacement") {
    shiny::tagList(
      shiny::h5("Ratio of Hires"),
      shiny::h5(paste("to Separations:", rate))
    )
  } else {
    shiny::tagList(
      shiny::h5(paste("Count:", values[["count"]])),
      shiny::h5(paste("Rate:", rate))
    )
  }

  shiny::renderUI({
    bslib::value_box(
      title = stringr::str_to_title(movement_type),
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
          "Reference period: ",
          format(as.Date(values[["ref_date"]]), "%b %Y")
        ),
      ),
      bslib::popover(
        bsicons::bs_icon("info-circle-fill"),
        placement = "left",
        switch(
          movement_type,
          hire = "Number of personnel active in the reference period but not in the previous one, and their share of the previous period's active headcount.",
          separation = "Number of personnel active in the previous period but no longer active in the reference period, other than retirees, and their share of the previous period's active headcount.",
          retirement = "Number of personnel active in the previous period who are no longer active in the reference period and are recorded as pensioners, and their share of the previous period's active headcount.",
          replacement = "Ratio of hires to separations. Retirements are not separations, so they are left out. Above 1, hires outnumber separations."
        )
      )
    )
  })
}
