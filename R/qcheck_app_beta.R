#' Run the govhr Shiny Dashboard Application
#'
#' Launches an interactive Shiny dashboard for govhr data quality check.
#'
#' @param est_data Data frame with establishment attributes.
#' @param personnel_data Data frame with workforce/personnel attributes (headcount).
#' @param contract_data Data frame with contract/salary attributes (wage bill).
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @return A Shiny app object.
#'
#' @examples
#' \dontrun{
#' run_govhrapp_qcheck(est_data, personnel_data, contract_data)
#' }
#'
#' @importFrom shiny shinyApp addResourcePath
#' @importFrom bslib page_navbar nav_panel nav_spacer bs_theme bs_add_rules navbar_options font_google
#' @importFrom ggplot2 theme_set theme_minimal theme element_text update_geom_defaults
#' @importFrom thematic thematic_shiny
#' @importFrom lubridate year
#' @importFrom scales label_number cut_short_scale
#' @export
run_govhrapp_qcheck <- function(est_data, personnel_data, contract_data, ...) {
  # add path to visual assets (image and css)
  shiny::addResourcePath("assets", system.file("www", package = "govhrapp"))

  # ensure ggplot2 and plotly inherit bslib themes
  ggplot2::theme_set(
    ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 10.5)
      )
  )

  thematic::thematic_shiny(
    font = "auto",
    accent = "#C34729",
    sequential = "#C34729"
  )

  ggplot2::update_geom_defaults("point", list(colour = "#C34729"))
  ggplot2::update_geom_defaults("line",  list(colour = "#C34729"))
  ggplot2::update_geom_defaults("col",   list(fill   = "#C34729"))

  ui <- bslib::page_navbar(
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
        readLines(system.file("www/styles.css", package = "govhrapp"))
      ),

    navbar_options = navbar_options(
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
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, md = c(1, 10, 1), lg = c(1.5, 9, 1.5)),
        shiny::div(),
        bslib::card(
          bslib::card_header(
            shiny::tags$img(
              src = "assets/govhr_logo.png",
              style = "max-width: 1200px; display: block; margin-left: auto; margin-right: auto;",
              width = "80%"
            )
          ),
          bslib::card_body(
            shiny::tags$div(
              style = "max-width: 800px; margin: 0 auto; padding: 2rem 3rem;",
              shiny::tags$h3("Welcome to govhr."),
              shiny::markdown(
                readLines(system.file("markdown/qcheck_home.md", package = "govhrapp"))
              )
            )
          )
        ),
        shiny::div()
      )
    ),
    
    # panel 2: coverage
    bslib::nav_panel(
      "Coverage",
      icon = shiny::icon("building"),

      # content
      coverage_ui("coverage", est_data, personnel_data, contract_data)
    )
  )

  server <- function(input, output, session) {
    coverage_server("coverage", est_data, personnel_data, contract_data)
  }

  shiny::shinyApp(ui, server, ...)
}
