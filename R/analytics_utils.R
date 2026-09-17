#' Render a Workforce Movement Value Box
#'
#' Renders the count and rate of one movement type at the latest reference
#' period. Values are read from the analytics cache when available and computed
#' from `.data` otherwise.
#'
#' @param .data Data frame containing personnel data.
#' @param movement_type Character. One of `"hire"`, `"fire"`, `"retirement"` or
#'   `"turnover"`.
#' @param cache List of pre-computed summaries from [build_analytics_cache()],
#'   or `NULL` to compute the values directly.
#'
#' @return A Shiny render function producing the value box.
#'
#' @importFrom bsicons bs_icon
#' @importFrom bslib popover value_box value_box_theme
#' @importFrom purrr pluck
#' @importFrom shiny h5 renderUI tagList
#' @importFrom stringr str_to_title
#' @keywords internal
render_movement_box <- function(.data, movement_type, cache = NULL) {
  values <- purrr::pluck(cache, "workforce", "movement_box", movement_type)

  if (is.null(values)) {
    values <- summarise_movement_box(.data, movement_type)
  }

  box_value <- if (movement_type == "turnover") {
    shiny::tagList(
      shiny::h5("Ratio of Hires"),
      shiny::h5(paste("to Exits:", round(values[["rate"]], 3)))
    )
  } else {
    shiny::tagList(
      shiny::h5(paste("Count:", values[["count"]])),
      shiny::h5(paste("Rate:", round(values[["rate"]], 3), "%"))
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
          fire = "person-dash-fill",
          retirement = "person-badge-fill",
          turnover = "arrow-repeat"
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
          hire = "Number and share of personnel hired in the most recent reference period.",
          fire = "Number and share of personnel separated (voluntary and involuntary) in the most recent reference period.",
          retirement = "Number and share of personnel retired in the most recent reference period.",
          turnover = "Ratio of hires to separations (including retirements) in the most recent reference period."
        )
      )
    )
  })
}
