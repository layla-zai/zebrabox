#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# ZEBRA APP

library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "ZebraBox Data Upload",
  sidebar = sidebar(
    # Upload Data file input
    fileInput("data_file", "Upload Data:"),
    
    # Upload Metadata file input + Download template button
    fileInput("metadata_file", "Upload Metadata:"),
    downloadButton("download_template", "Download template"),
    
    # Optional text input for metadata order
    textInput("metadata_order", "(optional) Specify metadata order:"),
    
    # Run button
    actionButton("run", "Run")
  ),
  
  card(
    
    conditionalPanel(
      condition = "input.run == 0",
      tags$h3("Please upload your data and click Run to begin.")
    ),
    
    conditionalPanel(
      condition = "input.run > 0",
      fluidRow(
        column(
          width = 3,
          card(
            downloadButton("download_report", "Download Report", class = "btn-sm mb-2"),
            downloadButton("download_data", "Download Data", class = "btn-sm")
          )
        ),
        column(
          width = 9,
          navset_card_underline(
            nav_panel("Plot 1", plotOutput("plot1")),
            nav_panel("Plot 2", plotOutput("plot2")),
            nav_panel("Plot 3", plotOutput("plot3")),
            nav_panel("Plot 4", plotOutput("plot4"))
          )
        )
      )
    )
  ),
  
  theme = bs_theme(
    bg = "white",
    fg = "black",
    primary = "#E69F00",
    secondary = "#0072B2",
    success = "#009E73",
    base_font = font_google("Inter")
  )
)
  
server <- function(input, output, session) {
  
  output$download_template <- downloadHandler(
    filename = function() "metadata_template.csv",
    content = function(file) {
      write.csv(data.frame(SampleID = "", Condition = ""), file, row.names = FALSE)
    }
  )
  
  output$download_report <- downloadHandler(
    filename = function() "zebrabox_report.txt",
    content = function(file) {
      writeLines("This is your ZebraBox analysis report.", file)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() "processed_data.tsv",
    content = function(file) {
      # For now, just write empty placeholder data
      write.table(data.frame(), file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
  
  # Placeholder empty plots just to make UI look right
  output$plot1 <- renderPlot({ plot.new() })
  output$plot2 <- renderPlot({ plot.new() })
  output$plot3 <- renderPlot({ plot.new() })
  output$plot4 <- renderPlot({ plot.new() })
  
}

shinyApp(ui, server)