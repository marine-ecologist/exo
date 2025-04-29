library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(conflicted)

# Label dictionary
param_labels <- c(
  chlorophyll_rfu = "Chlorophyll (RFU)",
  chlorophyll_rfu_1 = "Chlorophyll Backup (RFU)",
  chlorophyll_ug_l = "Chlorophyll (\u00b5g/L)",
  chlorophyll_ug_l_1 = "Chlorophyll Backup (\u00b5g/L)",
  cond_u_s_cm = "Conductivity (\u00b5S/cm)",
  depth_m = "Depth (m)",
  f_dom_qsu = "fDOM (QSU)",
  f_dom_rfu = "fDOM (RFU)",
  n_lf_cond_u_s_cm = "LF Conductivity (\u00b5S/cm)",
  odo_sat = "O\u2082 Saturation (%)",
  odo_local = "O\u2082 Local (%)",
  odo_mg_l = "O\u2082 (mg/L)",
  pressure_psi_a = "Pressure (psiA)",
  sal_psu = "Salinity (PSU)",
  sp_cond_u_s_cm = "Specific Conductivity (\u00b5S/cm)",
  bga_pe_rfu = "BGA-PE (RFU)",
  bga_pe_rfu_1 = "BGA-PE Backup (RFU)",
  bga_pe_ug_l = "BGA-PE (\u00b5g/L)",
  bga_pe_ug_l_1 = "BGA-PE Backup (\u00b5g/L)",
  tds_mg_l = "TDS (mg/L)",
  turbidity_fnu = "Turbidity (FNU)",
  tss_mg_l = "TSS (mg/L)",
  wiper_position_volt = "Wiper Position (V)",
  temp_c = "Temperature (\u00b0C)",
  vertical_position_m = "Vertical Position (m)",
  battery_v = "Battery (V)",
  cable_pwr_v = "Cable Power (V)"
)

# UI
ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")),
  tags$style(HTML(".sidebar-custom { background-color: #f5f5f5; padding: 15px; border-radius: 5px; border: 1px solid #ddd; }")),
  titlePanel("ExoViewer timeseries"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "sidebar-custom",
          fileInput("custom_file", "Add CSV file"),
          selectizeInput(
            "parameters", "Parameters:",
            choices = setNames(names(param_labels), param_labels),
            selected = "temp_c", multiple = TRUE,
            options = list(
              'plugins' = list('remove_button'),
              'create' = TRUE,
              'persist' = FALSE
            )
          ),
          checkboxInput("show_points", "Show points", value = FALSE)
      )
    ),
    mainPanel(
      plotlyOutput("ts_plot", height = "1000px")
    )
  )
)

server <- function(input, output, session) {
  load_data <- reactive({
    req(input$custom_file)
    file <- input$custom_file$datapath

    read.csv(file, skip = 9, fileEncoding = "UTF-16LE") |>
      janitor::clean_names(replace = c("\u00b5" = "u")) |>
      mutate(datetime = lubridate::dmy(date_mm_dd_yyyy) + lubridate::hms(time_hh_mm_ss))
  })

  output$ts_plot <- renderPlotly({
    req(input$custom_file, input$parameters)

    df <- tryCatch({
      load_data() |>
        select(datetime, all_of(input$parameters)) |>
        pivot_longer(-datetime, names_to = "parameter", values_to = "value")
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(df)) {
      return(
        plotly::plotly_empty() |>
          layout(title = list(
            text = "Parameters not found in Exo data",
            font = list(size = 22),
            yref = "paper"
          ))
      )
    }

    plots <- map(input$parameters, function(p) {
      df_sub <- df |> filter(parameter == p)
      plot_ly(
        data = df_sub,
        x = ~datetime,
        y = ~value,
        type = "scatter",
        mode = if (input$show_points) "lines+markers" else "lines",
        opacity = 1,
        name = param_labels[[p]],
        hovertemplate = paste0("<b>", param_labels[[p]], "</b><br>",
                               "Time: %{x|%Y-%m-%d %H:%M:%S}<br>",
                               "Value: %{y:.2f}<extra></extra>")
      )
    })

    subplot_plot <- subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE)

    layout_args <- list(
      uirevision = "preserve",
      dragmode = "zoom",
      hovermode = "x unified",
      xaxis = list(rangeslider = list(visible = TRUE), fixedrange = FALSE)
    )

    for (i in seq_along(plots)) {
      yname <- if (i == 1) "yaxis" else paste0("yaxis", i)
      layout_args[[yname]] <- list(fixedrange = FALSE)
    }

    do.call(plotly::layout, c(list(subplot_plot), layout_args))
  })
}

shinyApp(ui, server)
