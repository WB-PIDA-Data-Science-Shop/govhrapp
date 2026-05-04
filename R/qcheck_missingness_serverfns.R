#' Missingness Server Module
#'
#' Server logic for the Missingness section, processing and visualizing missing data patterns.
#'
#' @param id Character string. The module namespace ID.
#' @param qc_obj A quality control object from \code{govhr::compute_qualitycontrol()}.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @importFrom shiny moduleServer req observe observeEvent updateSelectInput
#' @importFrom data.table copy
#' @importFrom stats setNames
#' @importFrom highcharter renderHighchart highchart hc_chart hc_xAxis hc_yAxis hc_add_series hc_title hc_subtitle hc_tooltip hc_colorAxis hc_legend
#'
#' @keywords internal
missingness_server <- function(id, qc_obj) {

  shiny::moduleServer(id, function(input, output, session) {

    overall_dt <- qc_obj$missingness$overall
    group_dt   <- qc_obj$missingness$group

    # --- Populate module choices ---
    shiny::observe({
      modules <- sort(unique(overall_dt$module))
      labels  <- paste0(toupper(substr(modules, 1, 1)), substring(modules, 2))
      shiny::updateSelectInput(session, "module",
                               choices  = stats::setNames(modules, labels),
                               selected = modules[1])
    })

    # --- Update group_var choices when module changes ---
    shiny::observeEvent(input$module, {
      req(input$module)
      grp <- unique(group_dt[group_dt$module == input$module,
                              c("group_var", "group_label")])
      grp <- grp[order(grp$group_var), ]
      shiny::updateSelectInput(session, "group_var",
                               choices  = stats::setNames(grp$group_var,
                                                          grp$group_label),
                               selected = grp$group_var[1])
    })

    # --- Card 1: Overall missingness bar chart ---
    output$overall_bar <- highcharter::renderHighchart({
      shiny::req(input$module)

      plot_dt <- overall_dt[overall_dt$module == input$module & overall_dt$pct_missing > 0, ]
      plot_dt <- plot_dt[order(-plot_dt$pct_missing), ]

      if (nrow(plot_dt) == 0) return(NULL)

      bar_colors <- ifelse(plot_dt$pct_missing >= 0.5,  "#d32f2f",
                    ifelse(plot_dt$pct_missing >= 0.2,  "#ff9800", "#4caf50"))

      highcharter::highchart() |>
        highcharter::hc_chart(type = "bar") |>
        highcharter::hc_xAxis(
          categories = plot_dt$target_label,
          title      = list(text = "Field")
        ) |>
        highcharter::hc_yAxis(
          title  = list(text = "% Missing"),
          labels = list(format = "{value}%"),
          max    = 100
        ) |>
        highcharter::hc_add_series(
          data = lapply(seq_len(nrow(plot_dt)), function(i) {
            list(y     = round(plot_dt$pct_missing[i] * 100, 1),
                 color = bar_colors[i])
          }),
          type         = "bar",
          name         = "Missing",
          showInLegend = FALSE
        ) |>
        highcharter::hc_title(text = paste0(
          toupper(substr(input$module, 1, 1)), substring(input$module, 2),
          " -- Overall Missingness by Field"
        )) |>
        highcharter::hc_tooltip(pointFormat = "<b>{point.y:.1f}%</b> missing") |>
        highcharter::hc_legend(enabled = FALSE) |>
        add_export_menu()
    })

    # --- Card 2: Grouped heatmap ---
    output$missingness_heatmap <- highcharter::renderHighchart({
      shiny::req(input$module, input$group_var, input$top_n, input$sort_by)

      plot_dt <- data.table::copy(
        group_dt[group_dt$module    == input$module &
                 group_dt$group_var == input$group_var, ]
      )
      if (nrow(plot_dt) == 0) return(NULL)

      # Aggregate to pick top N group_vals
      agg <- plot_dt[, .(mean_missing = sum(n_missing) / sum(N)), by = .(group_val)]

      if (input$sort_by == "desc") {
        agg <- agg[order(-mean_missing)]
      } else if (input$sort_by == "asc") {
        agg <- agg[order(mean_missing)]
      } else {
        agg <- agg[order(group_val)]
      }

      top_vals <- agg[seq_len(min(input$top_n, .N))]$group_val
      plot_dt  <- plot_dt[group_val %in% top_vals]

      # Preserve sort order for x-axis
      group_vals   <- top_vals[top_vals %in% unique(plot_dt$group_val)]
      target_labels <- sort(unique(plot_dt$target_label))
      grp_label    <- plot_dt$group_label[1]

      heatmap_data <- plot_dt[, .(
        x     = match(group_val,    group_vals)    - 1L,
        y     = match(target_label, target_labels) - 1L,
        value = round(pct_missing * 100, 1)
      )]

      highcharter::highchart() |>
        highcharter::hc_chart(type = "heatmap") |>
        highcharter::hc_xAxis(
          categories = as.character(group_vals),
          title      = list(text = grp_label)
        ) |>
        highcharter::hc_yAxis(
          categories = as.character(target_labels),
          title      = list(text = "Field")
        ) |>
        highcharter::hc_add_series(
          data       = heatmap_data,
          type       = "heatmap",
          name       = "Missing %",
          dataLabels = list(
            enabled = TRUE,
            format  = "{point.value:.0f}%",
            style   = list(fontSize = "10px", fontWeight = "normal",
                           textOutline = "none")
          )
        ) |>
        highcharter::hc_colorAxis(
          min   = 0,
          max   = 100,
          stops = list(
            list(0,    "#FFFFFF"),
            list(0.25, "#FEF0D9"),
            list(0.5,  "#FDCC8A"),
            list(0.75, "#FC8D59"),
            list(1,    "#D7301F")
          ),
          labels = list(format = "{value}%")
        ) |>
        highcharter::hc_title(
          text = paste0("Missingness by ", grp_label)
        ) |>
        highcharter::hc_subtitle(
          text = paste0(
            "Top ", length(group_vals), " groups -- sorted by ",
            ifelse(input$sort_by == "desc", "highest",
            ifelse(input$sort_by == "asc",  "lowest", "alphabetical")),
            " overall missingness"
          )
        ) |>
        highcharter::hc_tooltip(
          pointFormat = paste0(
            "<b>", grp_label, ":</b> {point.x}<br>",
            "<b>Field:</b> {point.y}<br>",
            "<b>Missing:</b> {point.value:.1f}%"
          )
        ) |>
        add_export_menu()
    })

  })
}
