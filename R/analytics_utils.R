#' Format a Movement Rate for Display
#'
#' @param rate Numeric. A rate as a proportion, or the replacement ratio.
#' @param movement_type Character. One of `"hire"`, `"separation"`,
#'   `"retirement"` or `"replacement"`.
#'
#' @return Character. A percentage such as `"3.1%"`, or for `"replacement"` a
#'   ratio such as `"1.49"`.
#'
#' @importFrom scales label_number label_percent
#' @keywords internal
format_movement_rate <- function(rate, movement_type) {
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
#' @importFrom govhr compute_movement
#' @importFrom purrr pluck
#' @importFrom shiny h5 renderUI tagList
#' @importFrom stringr str_to_title
#' @keywords internal
render_movement_box <- function(.data, movement_type, cache = NULL) {
  values <- purrr::pluck(cache, "workforce", "movement_box", movement_type)

  if (is.null(values)) {
    values <- summarise_movement_box(
      govhr::compute_movement(.data),
      compute_retirement(.data),
      movement_type
    )
  }

  rate <- format_movement_rate(values[["rate"]], movement_type)

  box_value <- if (movement_type == "replacement") {
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
          hire = "Number of personnel active in the most recent reference period but not in the previous one, and their share of active headcount.",
          separation = "Number of personnel active in the reference period but no longer active in the next one, for any reason including retirement, and their share of active headcount.",
          retirement = "Number of personnel active in the reference period who are pensioners in the next one, and their share of active headcount.",
          replacement = "Ratio of hires to separations (including retirements). Above 1, more personnel join than leave."
        )
      )
    )
  })
}
