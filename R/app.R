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
run_govhrapp <- function(...) {
  # add path to visual assets (image and css)
  shiny::addResourcePath("assets", "inst/www")

  ui <- bslib::page_navbar(
    title = "govhr dashboard",
    

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
      position = "fixed-top",
      collapse_below = "xl",
      underline = TRUE
    ),

    padding = "20px",

    # custom CSS
    # shiny::tags$head(shiny::includeCSS("www/styles.css")),

    # panel 1: home
    bslib::nav_panel(
      "Home",
      icon = shiny::icon("home"),

      # content
      shiny::div(
        style = "padding: 40px 20px;",
        bslib::card(
          bslib::card_header(
            shiny::tags$img(src = "assets/govhr_logo.png", width = "80%")
          ),
          bslib::card_body(
            shiny::tags$br(),
            shiny::tags$h3("Welcome to govhr."),
            shiny::markdown(
              readLines("inst/markdown/home.md")
              )
            )
          )
        )
      ),

    # panel 2: wage bill
    bslib::nav_panel(
      "Wage Bill",
      icon = shiny::icon("money-bill"),

      # content
      div(
        style = "padding: 40px 20px;",
        bslib::card(
          bslib::card_header(
            "Wage Bill"
          ),
          bslib::card_body(
            shiny::markdown(
              readLines("inst/markdown/wagebill.md")
            )
          )
        )
      )
      )
    )

  server <- function(input, output, session) {}

  shiny::shinyApp(ui, server, ...)
}