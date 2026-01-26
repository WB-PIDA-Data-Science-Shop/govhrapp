#' Run the govhr Shiny Dashboard Application
#'
#' Launches an interactive Shiny dashboard for govhr data visualization and analysis.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @return A Shiny app object.
#'
#' @examples
#' \dontrun{
#' run_govhrapp()
#' }
#'
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @import dplyr
#' @importFrom thematic thematic_shiny
#' @importFrom lubridate year
run_govhrapp <- function(...) {
  # add path to visual assets (image and css)
  shiny::addResourcePath("assets", "inst/www")

  # ensure ggplot2 and plotly inherit bslib themes
  ggplot2::theme_set(
    ggplot2::theme_minimal()
  )

  thematic::thematic_shiny(font = "auto")

  # globals
  personnel_data <- govhrapp::personnel_data

  ui <- bslib::page_navbar(
    title = "govhr dashboard",
    fillable = FALSE,

    # set theme
    theme = bslib::bs_theme(
      bootswatch = "litera",
      base_font = font_google("Source Sans Pro"),
      code_font = font_google("Source Sans Pro"),
      heading_font = font_google("Fira Sans"),
      navbar_bg = "#FFFFFF"
    ) |>
      bslib::bs_add_rules(
        readLines("inst/www/styles.css")
      ),

    navbar_options = navbar_options(
      underline = TRUE
    ),

    padding = "20px",

    # panel 1: home
    bslib::nav_panel(
      "Home",
      icon = shiny::icon("home"),

      # content
      bslib::card(
        bslib::card_header(
          shiny::tags$img(
            src = "assets/govhr_logo.png", 
            style = "display: block; margin-left: auto; margin-right: auto;",
            width = "80%"
          )
        ),
        bslib::card_body(
          shiny::tags$br(),
          shiny::tags$h3("Welcome to govhr."),
          shiny::markdown(
            readLines("inst/markdown/home.md")
          )
        )
      )
    ),

    # panel 2: wage bill
    bslib::nav_panel(
      "Wage Bill",
      icon = shiny::icon("money-bill"),
      wagebill_ui("wagebill", personnel_data)
    ),

    # panel 3: workforce planning
    bslib::nav_panel(
      "Workforce",
      icon = shiny::icon("person-walking"),
      workforce_ui("workforce", personnel_data)
    )
  )

  server <- function(input, output, session) {
    wagebill_server("wagebill", personnel_data)
    workforce_server("workforce", personnel_data)
  }

  shiny::shinyApp(ui, server, ...)
}
