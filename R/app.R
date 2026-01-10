library(shiny)
library(bs4Dash)

# add path to visual assets (image and css)
addResourcePath("assets", "www")

ui <- dashboardPage(
  
  freshTheme = fresh::create_theme(
    fresh::bs4dash_layout(sidebar_width = "350px")
  ),

# header -----------------------------------------------------------------
  dashboardHeader(
    title = bs4Dash::dashboardBrand(
        title = "govhr dashboard"
      ),
      status = "white",
      border = TRUE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE
  ),

# sidebar ----------------------------------------------------------------
  dashboardSidebar(
     status = "info",
      skin = "light",
      elevation = 5,
      
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home"))
      )
    ),

# main body --------------------------------------------------------------
  dashboardBody(
    tags$head(includeCSS("www/styles.css")),
      
      tabItems(
        tabItem(
          tabName = "home",
          
          bs4Dash::bs4Card(
            width = 12,
            status = "navy",
            solidHeader = TRUE,
            title =
              span(
                img(src = "assets/govhr_logo.png", width = "80%")
              ),
            br(),
            p("Welcome to the govhr dashboard.")
          )
        )
      )
  )
)

server <- function(input, output, session){

}

shinyApp(ui, server)