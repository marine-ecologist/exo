library(shiny)
library(leaflet)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyverse)
library(janitor)
library(ggplot2)

exo <- read.csv("KorEXO Measurement File Export - 031824 030139.csv") |>
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
  mutate(
    max_val_row = which.max(vertical_position_m), # Identify the row with the maximum 'vertical_position_m' value
    cast = ifelse(row_number() <= max_val_row, "descent", "ascent") # Assign 'descent' or 'ascent'
  ) |> 
  select(-max_val_row) # drop temp col



# Assuming 'exo' is your dataframe as prepared above
# You must ensure 'time' column is in POSIXct format for sliderInput to work correctly
exo$time <- as.POSIXct(exo$time)
time_range <- range(exo$time, na.rm = TRUE)
y_axis_choices <- setdiff(colnames(exo), c("sample_id", "time", "vertical_position_m", 
                                           "gps_latitude", "gps_longitude", "altitude_m", 
                                           "barometer_mm_hg", "seq_id", "cast"))

# Define custom icons for selected and unselected states using awesomeIcons
selected_icon <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "red"
)

unselected_icon <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "blue"
)


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
                          choices = y_axis_choices, selected = y_axis_choices[1]))
  )
)
server <- function(input, output, session) {
  # Reactive expression for filtered data
  filtered_data <- reactive({
    exo %>% 
      filter(time >= input$timeRange[1] & time <= input$timeRange[2])
  })

    
  # Map output
  output$map <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addMarkers(~gps_longitude, ~gps_latitude, popup = ~sample_id,
                       layerId = ~sample_id, group = ~sample_id) %>%
      addProviderTiles('Esri.WorldImagery')
  })
  
  # Reactive values for selected sample_id
  selected_sample_id <- reactiveValues(sample_id = character(0))
  
  # Update selected sample_id on marker click
  observeEvent(input$map_marker_click, {
    site_clicked <- input$map_marker_click$id
    if(site_clicked %in% selected_sample_id$sample_id) {
      selected_sample_id$sample_id <- selected_sample_id$sample_id[selected_sample_id$sample_id != site_clicked]
    } else {
      selected_sample_id$sample_id <- c(selected_sample_id$sample_id, site_clicked)
    }
  })
  
  # Reactive value to store selected sample IDs
  selected_ids <- reactiveValues(ids = character(0))
  
  
  # Plot output
  output$plot <- renderPlot({
    site_data <- filtered_data() %>% 
      filter(sample_id %in% selected_sample_id$sample_id) |> 
      group_by(sample_id) |> 
      mutate(means = zoo::rollmean(.data[[input$yAxis]], k = 3, fill = NA))
    if (nrow(site_data) > 0) {
      ggplot(site_data, aes(x = vertical_position_m, y = .data[[input$yAxis]], color = factor(sample_id))) +
        geom_point() +
        geom_line(linewidth=0.1) +
        geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se=FALSE) +
        theme_bw() +
        labs(y = input$yAxis, color = "Sample ID")
    } else {
      ggplot() + theme_bw() +labs(y = input$yAxis, color = "Sample ID")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)