
library(shiny)
library(leaflet)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyverse)
library(janitor)
library(ggplot2)

exo <- read.csv("/Users/rof011/Desktop/KorEXO Measurement File Export - 031824 030139.csv") |>
  slice(8:n()) |> # cut the dumb headers
  mutate(across(everything(), ~str_replace_all(., "\\xb5S/", ""))) |> # replace the annoying UTF-8 encoding
  row_to_names(1) |> # make the first row the header
  clean_names(replace = c("`\`" = "")) |> # clean new rownames 
  na.omit() |> # drop columns with missing data
  filter(if_all(everything(), ~!is.na(.) & . != "")) |> # drop rows with missing data |> 
  unite("time", date_mm_dd_yyyy:time_hh_mm_ss, sep=" ", remove=FALSE) |> # combine date and time
  mutate(is_header = row_number() == 1 | str_detect(date_mm_dd_yyyy, "^Date")) |> # if first row or contains date 
  group_by(grp = cumsum(is_header)) |> 
  filter(!str_detect(date_mm_dd_yyyy, "Date")) |> # drop rows containing date
  mutate(sample_id = if_else(row_number() == 1, paste0(date_mm_dd_yyyy, "_", time_hh_mm_ss), NA_character_)) |> # make NA if else
  fill(sample_id, .direction = "down") |> # fill down
  ungroup() |> 
  mutate(time=dmy_hms(time)) |> # lubridate for time 
  select(-date_mm_dd_yyyy, -time_hh_mm_ss) |>  # drop old date and tiem
  select(-is_header, -grp) |>   # remove helper columns
  relocate(sample_id) |>  # move id to first column
  select(- time_fract_sec, -site_name, -time_fract_sec, -wiper_position_volt, -battery_v, -cable_pwr_v, -upc) |> # clean redundancy
  group_by(sample_id) |> 
  mutate(seq_id=seq(1:n())) |> # add a sequence id per cast
  ungroup() |> 
  mutate(sample_id=as.factor(sample_id)) |> # make sample_id a factor
  mutate_if(is.character, as.numeric) |> # change character to numeric
  filter(vertical_position_m > 0) |> # remove air casts
  relocate(vertical_position_m, .after=time) |> # move earlier in column order 
  group_by(sample_id) |> 
  #set random colors:
  mutate(color = as.factor(sample_id)) |> 
  mutate(color = factor(color, levels = levels(color), labels = sample(hcl.colors(100, palette = "viridis"), length(levels(color)), replace = FALSE))) |> 
  mutate(
    max_val_row = which.max(vertical_position_m), # Identify the row with the maximum 'vertical_position_m' value
    cast = ifelse(row_number() <= max_val_row, "descent", "ascent") # Assign 'descent' or 'ascent'
  ) |> 
  select(-max_val_row) # drop temp col


# Convert 'time' to POSIXct if not already
exo$time <- as.POSIXct(exo$time)
time_range <- range(exo$time, na.rm = TRUE)

# Prepare choices for dropdown, excluding specified columns
y_axis_choices <- setdiff(colnames(exo), c("sample_id", "time", "vertical_position_m", 
                                           "gps_latitude", "gps_longitude", "altitude_m", 
                                           "barometer_mm_hg", "seq_id", "cast", "color"))

# UI definition
ui <- fluidPage(
  titlePanel("exoViewer"),
  fluidRow(
    column(6, leafletOutput("map", height = "600px")),
    column(6, plotOutput("plot", height = "600px"))
  ),
  fluidRow(
    column(4, sliderInput("timeRange", "Select Time Range:",
                          min = time_range[1], max = time_range[2],
                          value = time_range, timeFormat="%Y-%m-%d %H:%M:%S",
                          step = 60 * 5)),
    column(4, selectInput("yAxis", "Choose Y-axis Variable:", 
                          choices = y_axis_choices, selected = y_axis_choices[1])),
    column(4, actionButton("clearSelection", "Clear Selection"))
  )
)




server <- function(input, output, session) {
  # Reactive expression for filtered data
  filtered_data <- reactive({
    exo %>% 
      filter(time >= input$timeRange[1] & time <= input$timeRange[2])
  })
  
  # Reactive value to store selected sample IDs
  selected_sample_id <- reactiveValues(ids = character(0))
  
  # Update selected sample_id on marker click
  observeEvent(input$map_marker_click, {
    site_clicked <- input$map_marker_click$id
    if(site_clicked %in% selected_sample_id$ids) {
      selected_sample_id$ids <- selected_sample_id$ids[selected_sample_id$ids != site_clicked]
    } else {
      selected_sample_id$ids <- c(selected_sample_id$ids, site_clicked)
    }
    # Force map to redraw
    output$map <- renderLeaflet({
      redrawMap()
    })
  })
  
  # Function to redraw map with updated colors based on selection
  redrawMap <- reactive({
    
    #print(selected_sample_id$ids)
    data <- filtered_data()
    data$selected_color <- ifelse(data$sample_id %in% selected_sample_id$ids, paste0(data$color), "#FFFFFF") # Default color for unselected
   
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(~gps_longitude, ~gps_latitude, popup = ~sample_id, color = "black",
                       layerId = ~sample_id, group = ~sample_id, radius = 6,
                       fill = TRUE, fillColor = ~selected_color, fillOpacity = 0.9, weight = 2) %>%
      addProviderTiles('Esri.WorldImagery')
  })
  
  # Map output with dynamic color change based on selection
  output$map <- renderLeaflet({
    redrawMap()
  })
  
  observeEvent(input$clearSelection, {
    selected_sample_id$ids <- character(0)
    redrawMap()
  })
  
  # Plot output remains unchanged
  output$plot <- renderPlot({
    site_data <- filtered_data() %>% 
      filter(sample_id %in% selected_sample_id$ids) |> 
      group_by(sample_id) |> 
      mutate(means = zoo::rollmean(.data[[input$yAxis]], k = 3, fill = NA))
    if (nrow(site_data) > 0) {
      ggplot(site_data, aes(x = vertical_position_m, y = .data[[input$yAxis]], color = color)) +
        geom_point() +
        geom_line() +
        geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se=FALSE) +
        scale_color_identity() +
        theme_bw() +
        labs(y = input$yAxis, color = "Sample ID")
    } else {
      ggplot() + theme_bw() + labs(y = input$yAxis, color = "Sample ID")
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
