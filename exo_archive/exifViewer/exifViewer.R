library(shiny)
install_exiftool()
library(exiftoolr)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .container-fluid {
        max-width: 1200px;
        overflow-x: auto;
        white-space: nowrap;
      }
    "))
  ),
  titlePanel("exifViewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Choose JPEG File', multiple=TRUE,
                accept = c('image/jpeg', 'image/jpg')
      )
    ),
    mainPanel(
      tableOutput('exifData')
    )
  )
)

server <- function(input, output) {
  output$exifData <- renderTable({
    inFile <- input$file
    if (is.null(inFile)) {
      return()
    }
    exif_data <- exif_read(inFile$datapath)
    return(exif_data)
  }, options = list(scrollX = TRUE))
}

shinyApp(ui = ui, server = server)