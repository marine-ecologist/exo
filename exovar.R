library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(conflicted)

# Parameter labels
param_labels <- c(
  chlorophyll_rfu = "Chlorophyll (RFU)",
  chlorophyll_rfu_1 = "Chlorophyll Backup (RFU)",
  chlorophyll_ug_l = "Chlorophyll (µg/L)",
  chlorophyll_ug_l_1 = "Chlorophyll Backup (µg/L)",
  cond_u_s_cm = "Conductivity (µS/cm)",
  depth_m = "Depth (m)",
  f_dom_qsu = "fDOM (QSU)",
  f_dom_rfu = "fDOM (RFU)",
  n_lf_cond_u_s_cm = "LF Conductivity (µS/cm)",
  odo_sat = "O₂ Saturation (%)",
  odo_local = "O₂ Local (%)",
  odo_mg_l = "O₂ (mg/L)",
  pressure_psi_a = "Pressure (psiA)",
  sal_psu = "Salinity (PSU)",
  sp_cond_u_s_cm = "Specific Conductivity (µS/cm)",
  bga_pe_rfu = "BGA-PE (RFU)",
  bga_pe_rfu_1 = "BGA-PE Backup (RFU)",
  bga_pe_ug_l = "BGA-PE (µg/L)",
  bga_pe_ug_l_1 = "BGA-PE Backup (µg/L)",
  tds_mg_l = "TDS (mg/L)",
  turbidity_fnu = "Turbidity (FNU)",
  tss_mg_l = "TSS (mg/L)",
  wiper_position_volt = "Wiper Position (V)",
  temp_c = "Temperature (°C)",
  vertical_position_m = "Vertical Position (m)",
  battery_v = "Battery (V)",
  cable_pwr_v = "Cable Power (V)"
)

ui <- fluidPage(
  titlePanel("EXO Regression Viewer"),
  sidebarLayout(
    sidebarPanel(
      div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
          fileInput("custom_file", "Upload CSV"),
          uiOutput("date_crop"),
          uiOutput("depth_crop")
      ),
      div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
          selectInput("x_var", "X Axis:", choices = setNames(names(param_labels), param_labels), selected = "depth_m"),
          uiOutput("x_crop")
      ),
      div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
          selectInput("y_var", "Y Axis:", choices = setNames(names(param_labels), param_labels), selected = "odo_sat"),
          uiOutput("y_crop")
      ),
      div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
          selectInput("fill_var", "Fill color:", choices = setNames(names(param_labels), param_labels), selected = "temp_c")
      ),
      div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
          selectInput("size_var", "Point size:", choices = c("None" = "None", setNames(names(param_labels), param_labels)), selected = "tds_mg_l")
      )
    ),
    mainPanel(
      plotlyOutput("regression_plot", height = "800px"),
      verbatimTextOutput("regression_stats")
    )
  )
)

server <- function(input, output, session) {
  load_data <- reactive({
    req(input$custom_file)
    file <- input$custom_file$datapath
    read.csv(file, skip = 9, fileEncoding = "UTF-16LE") |>
      janitor::clean_names(replace = c("µ" = "u")) |>
      mutate(datetime = dmy(date_mm_dd_yyyy) + hms(time_hh_mm_ss))
  })

  output$date_crop <- renderUI({
    req(load_data())
    rng <- range(load_data()$datetime, na.rm = TRUE)
    dateRangeInput("date_range", "Datetime Range", start = rng[1], end = rng[2])
  })

  output$depth_crop <- renderUI({
    req(load_data())
    if (!"depth_m" %in% names(load_data())) return(NULL)
    rng <- range(load_data()$depth_m, na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2])) return(NULL)
    sliderInput("depth_range", "Depth Range (m)", min = rng[1], max = rng[2], value = rng)
  })

  output$x_crop <- renderUI({
    req(load_data(), input$x_var)
    if (!input$x_var %in% names(load_data())) return(NULL)
    rng <- range(load_data()[[input$x_var]], na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2])) return(NULL)
    sliderInput("x_range", "X Axis Range", min = rng[1], max = rng[2], value = rng)
  })

  output$y_crop <- renderUI({
    req(load_data(), input$y_var)
    if (!input$y_var %in% names(load_data())) return(NULL)
    rng <- range(load_data()[[input$y_var]], na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2])) return(NULL)
    sliderInput("y_range", "Y Axis Range", min = rng[1], max = rng[2], value = rng)
  })

  output$regression_stats <- renderPrint({
    req(input$x_var, input$y_var, input$date_range)
    vars <- c("datetime", input$x_var, input$y_var, input$fill_var, "depth_m")
    if (input$size_var != "None") vars <- c(vars, input$size_var)
    vars <- intersect(vars, names(load_data()))
    df <- load_data() |> select(any_of(vars)) |> drop_na()
    df <- df |> filter(
      between(datetime, input$date_range[1], input$date_range[2]),
      between(.data[[input$x_var]], input$x_range[1], input$x_range[2]),
      between(.data[[input$y_var]], input$y_range[1], input$y_range[2])
    )
    if (!is.null(input$depth_range) && "depth_m" %in% names(df)) {
      df <- df |> filter(between(depth_m, input$depth_range[1], input$depth_range[2]))
    }
    if (nrow(df) < 2) return("Insufficient data for regression")
    fit <- lm(df[[input$y_var]] ~ df[[input$x_var]])
    summary_fit <- summary(fit)
    r2 <- summary_fit$r.squared
    pval <- coef(summary_fit)[2, 4]
    ptext <- if (pval < 1e-5) "< 0.00001" else sprintf("%.3g", pval)
    sprintf("R-squared: %.3f\nP-value: %s\nN: %d", r2, ptext, nrow(df))
  })

  output$regression_plot <- renderPlotly({
    req(input$x_var, input$y_var, input$fill_var, input$custom_file, input$date_range)
    vars <- c("datetime", input$x_var, input$y_var, input$fill_var, "depth_m")
    use_size <- input$size_var != "None"
    if (use_size) vars <- c(vars, input$size_var)
    vars <- intersect(vars, names(load_data()))
    df <- tryCatch({
      load_data() |> select(any_of(vars)) |> drop_na()
    }, error = function(e) {
      return(NULL)
    })
    if (is.null(df)) return(plotly::plotly_empty() |> plotly::layout(title = list(text = "Parameters not found", font = list(size = 22))))
    df <- df |> filter(
      between(datetime, input$date_range[1], input$date_range[2]),
      between(.data[[input$x_var]], input$x_range[1], input$x_range[2]),
      between(.data[[input$y_var]], input$y_range[1], input$y_range[2])
    )
    if (!is.null(input$depth_range) && "depth_m" %in% names(df)) {
      df <- df |> filter(between(depth_m, input$depth_range[1], input$depth_range[2]))
    }
    colnames(df)[2:4] <- c("x", "y", "fill")
    if (use_size) colnames(df)[5] <- "size"
    p <- ggplot(df, aes(x = x, y = y, color = fill)) +
      geom_point(aes(size = if (use_size) size else NULL), alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      labs(
        x = param_labels[[input$x_var]],
        y = param_labels[[input$y_var]],
        color = param_labels[[input$fill_var]],
        size = if (use_size) param_labels[[input$size_var]] else NULL
      ) +
      theme_minimal()
    ggplotly(p) |> plotly::layout(dragmode = "zoom", hovermode = "x unified", uirevision = "preserve")
  })
}

shinyApp(ui, server)
