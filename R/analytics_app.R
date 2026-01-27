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
#' @export
run_govhrapp <- function(...) {
  # add path to visual assets (image and css)
  shiny::addResourcePath("assets", "inst/www")

  # ensure ggplot2 and plotly inherit bslib themes
  ggplot2::theme_set(
    ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 12)
      )
  )

  thematic::thematic_shiny(
    font = "auto",
    accent = "#C34729",
    sequential = "#C34729"
  )

  # ggplot2::update_geom_defaults("point", list(colour = "#C34729"))
  # ggplot2::update_geom_defaults("line",  list(colour = "#C34729"))
  # ggplot2::update_geom_defaults("col",   list(fill   = "#C34729"))

  # globals
  personnel_data <- govhrapp::personnel_data

  ui <- bslib::page_navbar(
    # title = "govhr dashboard",
    fillable = FALSE,

    navbar_options = navbar_options(
      underline = TRUE
    ),

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

    padding = "10px",

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
    ),

    # panel 4: code
    nav_menu(
      title = "Code",
      icon = shiny::icon("github"),
      bslib::nav_item(
        shiny::tags$a(
          "govhr dashboard",
          href = "https://github.com/WB-PIDA-Data-Science-Shop/govhrapp",
          target = "_blank"
        )
      ),
      bslib::nav_item(
        shiny::tags$a(
          "govhr",
          href = "https://github.com/WB-PIDA-Data-Science-Shop/govhr",
          target = "_blank"
        )
      )
    )
  )

  server <- function(input, output, session) {
    wagebill_server("wagebill", personnel_data)
    workforce_server("workforce", personnel_data)
  }

  shiny::shinyApp(ui, server, ...)
}
