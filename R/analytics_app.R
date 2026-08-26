#' Run the govhr Shiny Dashboard Application
#'
#' Launches an interactive Shiny dashboard for govhr data visualization and analysis.
#'
#' @param workforce_data Data frame with workforce/personnel attributes (headcount).
#' @param wagebill_data Data frame with contract/salary attributes (wage bill).
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @return A Shiny app object.
#'
#' @examples
#' \dontrun{
#' run_govhrapp(workforce_data, wagebill_data)
#' }
#'
#' @importFrom shiny shinyApp addResourcePath useBusyIndicators
#' @importFrom bslib page_navbar nav_panel nav_spacer bs_theme bs_add_rules navbar_options font_google
#' @importFrom ggplot2 theme_set theme_minimal theme element_text update_geom_defaults
#' @importFrom thematic thematic_shiny
#' @importFrom lubridate year
#' @importFrom scales label_number cut_short_scale
#' @importFrom tidyr complete 
#' @export
run_govhrapp <- function(workforce_data, wagebill_data, ...) {
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
  ggplot2::update_geom_defaults("line", list(colour = "#C34729"))
  ggplot2::update_geom_defaults("col", list(fill = "#C34729"))

  # cache data to improve performance
  cache <- list(
    workforce_trend = workforce_data |>
      compute_trend_summary(
        group = "ref_date"
      ),
    wagebill_trend = wagebill_data |>
      compute_trend_summary(
        group = "ref_date",
        measure_col = "gross_salary_lcu"
      ),
    transfer_default = wagebill_data |>
      as.data.table() |>
      govhr:::detect_career_transitions(
        vars = "paygrade",
        decision_var = "base_salary_lcu"
      ) |>
      govhr::fastcount(
        dplyr::across(
          all_of(
            c("from", "to")
          )
        ),
        name = "transfer"
      ) |>
      tidyr::complete(
        .data[["from"]],
        .data[["to"]],
        fill = list(transfer = 0)
      )      
  )

  ui <- bslib::page_navbar(
    fillable = FALSE,

    header = shiny::useBusyIndicators(),

    navbar_options = navbar_options(
      underline = TRUE
    ),

    # set theme
    theme = bslib::bs_theme(
      bootswatch = "litera",
      base_font = font_google("Figtree", local = FALSE),
      code_font = font_google("Source Sans Pro", local = FALSE),
      heading_font = font_google("Libre Baskerville", local = FALSE),
      navbar_bg = "#ffffff"
    ) |>
      bslib::bs_add_rules(
        readLines(system.file("www/styles.css", package = "govhrapp"))
      ),

    padding = "10px",

    # panel 1: home
    bslib::nav_panel(
      "Home",
      icon = shiny::icon("home"),

      # content
      bslib::layout_columns(
        col_widths = bslib::breakpoints(
          sm = 12,
          md = c(1, 10, 1),
          lg = c(1.5, 9, 1.5)
        ),
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
                readLines(system.file(
                  "markdown/analytics_home.md",
                  package = "govhrapp"
                ))
              )
            )
          )
        ),
        shiny::div()
      )
    ),

    # panel 2: overview
    bslib::nav_panel(
      "Overview",
      icon = shiny::icon("gauge"),
      overview_ui("overview", workforce_data, wagebill_data)
    ),

    # panel 3: workforce planning
    bslib::nav_panel(
      "Workforce",
      icon = shiny::icon("person-walking"),
      workforce_ui("workforce", workforce_data, wagebill_data)
    ),

    # panel 4: wage bill
    bslib::nav_panel(
      "Wage Bill",
      icon = shiny::icon("money-bill"),
      wagebill_ui("wagebill", wagebill_data)
    ),

    # panel 5: code
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
    overview_server("overview", workforce_data, wagebill_data, cache = cache)
    wagebill_server(
      "wagebill",
      wagebill_data,
      cache = cache[["wagebill_trend"]]
    )
    workforce_server(
      "workforce",
      workforce_data,
      wagebill_data,
      cache = cache[c("workforce_trend", "transfer_default")]
    )
  }

  shiny::shinyApp(ui, server, ...)
}
