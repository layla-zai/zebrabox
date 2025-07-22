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
            nav_panel("Plate Map", plotlyOutput("plate_map")),
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
  
  metadata <- reactive({
    req(input$metadata_file)
    readxl::read_excel(input$metadata_file$datapath)
  })
  
  zebrabox_data <- reactive({
    req(input$data_file)
    raw_data <- read.delim(input$data_file$datapath, sep = "\t", header = TRUE)
    
    raw_data %>% 
      mutate(time_min = start / 60, 
             # experiment_time_of_day = '',
             period2 = ifelse(nchar(time_min) == 1, 1,
                              as.integer(str_extract(time_min, '^[0-9]')) + 1),
             plate = str_extract(location, 'Loc[AB]'),
             light = ifelse(period2 %% 2 != 0 & period2 != 1, 'dark', 'light')) %>%
      separate(aname, into = c('well', 'name'), sep = '_') %>%
      left_join(metadata(), by = join_by(well, plate == box_used)) %>%
      select(plate, treatment, well, time_min, light, period, 
             datatype, activity = actinteg, everything()) %>%
      filter(!is.na(treatment), timebinid == 1) 
  })
  
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
    content = function(file) {file.copy("C:/Users/layla/OneDrive/Documents/GitHub/zebrabox/0000-00-00_zebrabox_experiment_template.xlsx", file)
    }
  )
  
  output$plate_map <- plotly::renderPlotly({
    req(zebrabox_data())
    
    plate_map <- zebrabox_data() %>%
      separate(well, into = c('well_row', 'well_column'), sep = 1) %>%
      filter(datatype == 'QuantizationSum', period2 == 3) %>%
      ggplot(aes(x = well_column, y = well_row, text = treatment,
                 color = activity)) +
      geom_point(size = 4) +
      scale_color_continuous(limits = c(0, 7000)) +
      scale_x_discrete(position = "top")  +
      scale_y_discrete(limits = rev) +
      facet_wrap(~ plate, ncol = 2) +
      labs(x = NULL, y = NULL, color = 'Activity\n1st Dark\nPeriod') +
      theme_minimal()
    
    plotly::ggplotly(plate_map)
  })
  
  output$download_report <- downloadHandler(
    filename = function() "zebrabox_report.txt",
    content = function(file) {
      writeLines("This is your ZebraBox analysis report.", file)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() "processed_data.tsv",
    content = function(file) {
      write.table(data.frame(), file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
  
  output$plot1 <- renderPlot({
    data <- zebrabox_data()
    data %>%
      filter(datatype == 'QuantizationSum') %>%
      ggplot(aes(x = plate, y = activity)) +
      geom_boxplot(alpha = 0.5, fill = "orange") +
      geom_jitter(width = 0.2, alpha = 0.6) +
      labs(
        title = "Activity by Plate (LocA/LocB)",
        x = "Plate Location",
        y = "Activity (QuantizationSum)"
      ) +
      theme_classic(base_size = 14)
  })
  
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