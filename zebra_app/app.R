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
library(vroom)
library(dplyr)
library(ggplot2)
library(stringr)
library(writexl)
library(DT)
library(plotly)
library(tidyr)

ui <- page_sidebar(
  title = "ZebraBox Activity Analysis",
  sidebar = sidebar(
    fileInput("data_file", "Upload Data:"),
    fileInput("metadata_file", "Upload Metadata:"),
    downloadButton("download_template", "Download metadata template", class = "btn btn-secondary btn-md"),
    textInput("metadata_order", "(optional) Specify metadata order:"),
    actionButton("run", "Run", class = "btn btn-primary btn-lg")
  ),
  
  card(
    conditionalPanel(
      condition = "input.run == 0",
      tags$h5("Please upload your data and click Run to begin.")
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
            nav_panel("All Fish by Minute", plotOutput("plot1")),
            nav_panel("Condition Summary", plotOutput("plot2")),
            nav_panel("Plate Map / Well Activity", plotOutput("plot3")),
            nav_panel("Mean Activity", plotOutput("plot4")),
            nav_panel("Percent Change", plotOutput("plot5")),
            nav_panel("Activity by Period", plotOutput("plot6")),
            nav_panel("Percent Change by Period", plotOutput("plot7")),
            nav_panel("Statistical Summary", DT::dataTableOutput("stats_table")),
            nav_panel("Cleaned Data", DT::dataTableOutput("cleaned_table"))
          )
        )
      )
    )
  ),
  
  theme = bs_theme(
    bg = "#FAFAF8",
    fg = "black",
    primary = "#429E99",
    secondary = "#B2DFDB",
    success = "#00796B",
    base_font = font_google("Inter"),
    bootswatch = "flatly"
  )
)

server <- function(input, output, session) {
  
  metadata <- renderTable({
    req(input$metadata_file)
    df <- readxl::read_excel(input$metadata_file$datapath)
    df
  })
  
 reactive({read.delim(input$data_file$datapath, sep = "\t", header = TRUE) %>%
       mutate(
         time_min = start / 60,
         period2 = ifelse(time_min < 10, 1, floor(time_min / 10) + 1),
         plate = str_extract(location, 'Loc[AB]'),
         light = ifelse(period2 %% 2 != 0 & period2 != 1, 'dark', 'light'),
         well = str_extract(location, "[A-H][0-9]{2}")
       )}) -> zebrabox_data
 
  # zebrabox_data <- renderTable({
  #   req(input$data_file, input$metadata_file)
  #   
  #   raw_data <- read.delim(input$data_file$datapath, sep = "\t", header = TRUE)
  #   
  #   data <- raw_data %>%
  #     mutate(
  #       time_min = start / 60,
  #       period2 = ifelse(time_min < 10, 1, floor(time_min / 10) + 1),
  #       plate = str_extract(location, 'Loc[AB]'),
  #       light = ifelse(period2 %% 2 != 0 & period2 != 1, 'dark', 'light'),
  #       well = str_extract(location, "[A-H][0-9]{2}")
  #     )
  #   
  #   print("Data after mutate:")
  #   print(head(data))
  #   
  #   return(data)
  # })
    
  
  output$cleaned_table <- DT::renderDataTable({
    req(zebrabox_data())
    DT::datatable(zebrabox_data(),
                  extensions = 'Buttons',
                  options = list(dom = 'Blfrtip',
                                 buttons = c('copy', 'csv', 'excel'),
                                 lengthMenu = list(c(10, 25, 50, -1),
                                                   c(10, 25, 50, "All"))),
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: left; color:black; font-size:200%;',
                    'Zebrabox'))
  })
  
  output$download_template <- downloadHandler(
    filename = function() { 
      "0000-00-00_zebrabox_experiment_template.xlsx"
    },
    content = function(file) {
      file.copy("0000-00-00_zebrabox_experiment_template.xlsx", file)
    }
  )
  
  # output$plot1 <- renderPlot({
# data <- zebrabox_data()%>%
#     as.data.frame()
#   req(data)
#   
#   ggplot(data, aes(x = time_min, y = actinteg)) +
#     geom_point() +
#     labs(title = "Activity vs Time")
# })
  
  output$plot1 <- renderPlot({
    zebrabox_data() %>%
    ggplot(aes(x = time_min, y = actinteg)) +
      geom_point() +
      labs(title = "Activity vs Time")
  })
  
  
  # The rest of the plots can be placeholders for now
  output$plot2 <- renderPlot({ plot.new() })
  output$plot3 <- renderPlot({ plot.new() })
  output$plot4 <- renderPlot({ plot.new() })
  output$plot5 <- renderPlot({ plot.new() })
  output$plot6 <- renderPlot({ plot.new() })
  output$plot7 <- renderPlot({ plot.new() })
  
  output$stats_table <- DT::renderDataTable({
    data.frame(Message = "Statistics will appear here")
  })
  
}
shinyApp(ui, server)