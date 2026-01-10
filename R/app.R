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
#' run_govhr_app()
#' }
#'
#' @importFrom shiny addResourcePath shinyApp icon tags
#' @import bs4Dash
#' @importFrom fresh create_theme bs4dash_layout
#'
#' @export
run_govhr_app <- function(...) {
  # add path to visual assets (image and css)
  shiny::addResourcePath("assets", "www")

  ui <- bs4Dash::dashboardPage(
    freshTheme = fresh::create_theme(
      fresh::bs4dash_layout(sidebar_width = "350px")
    ),

    # header -----------------------------------------------------------------
    bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = "govhr dashboard"
      ),
      status = "white",
      border = TRUE,
      sidebarIcon = shiny::icon("bars"),
      controlbarIcon = shiny::icon("th"),
      fixed = FALSE
    ),

    # sidebar ----------------------------------------------------------------
    bs4Dash::dashboardSidebar(
      status = "info",
      skin = "light",
      elevation = 5,

      bs4Dash::sidebarMenu(
        bs4Dash::menuItem("Home", tabName = "home", icon = shiny::icon("home"))
      )
    ),

    # main body --------------------------------------------------------------
    bs4Dash::dashboardBody(
      shiny::tags$head(shiny::includeCSS("www/styles.css")),

      bs4Dash::tabItems(
        bs4Dash::tabItem(
          tabName = "home",

          bs4Dash::bs4Card(
            width = 12,
            status = "navy",
            solidHeader = TRUE,
            title = shiny::tags$span(
              shiny::tags$img(src = "assets/govhr_logo.png", width = "80%")
            ),
            shiny::tags$br(),
            shiny::tags$p("Welcome to the govhr dashboard.")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {}

  shiny::shinyApp(ui, server, ...)
}