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
library(tibble)
# library(rsconnect)
# options(RCurlOptions = list(ssl.verifypeer = FALSE))
# options(rsconnect.check.certificate = FALSE)
options(shiny.maxRequestSize = 30*1024^2)


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
            nav_panel("All Fish by Minute", plotlyOutput("all_fish_by_minute")),
            nav_panel("Condition Summary", plotlyOutput("condition_summary")),
            nav_panel("Plate Map / Well Activity", plotlyOutput("plate_map")),
            nav_panel("Mean Activity", plotOutput("mean_activity")),
            nav_panel("Percent Change", plotOutput("percent_change")),
            nav_panel("Activity by Period", plotOutput("activity_by_period")),
            nav_panel("Percent Change by Period", plotOutput("percent_change_by_period")),
            nav_panel("Statistical Summary", DT::dataTableOutput("stats_table")),
            nav_panel("Cleaned Data", DTOutput("cleaned_table"))
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
    readxl::read_excel("../2025-07-10_simulated_metadata.xlsx", skip = 1) %>%
      mutate(box_used = as.character(box_used),
             box_used = case_when(box_used == '1' ~ 'LocA',
                                  box_used == '2' ~ 'LocB',
                                  TRUE ~ NA_character_))
  })
  
  # Reactive expression for main data (zebrabox_data)
    zebrabox_data <- reactive({
      readr::read_tsv("../simulated_zebrabox_data2.tsv") %>% 
        mutate(time_min = start / 60,
              period2 = ifelse(time_min < 10, 1, floor(time_min / 10) + 1),
              plate = str_extract(location, 'Loc[AB]'),
              light = ifelse(period2 %% 2 != 0 & period2 != 1,
                              'dark', 'light')) %>%
        rename(activity = actinteg) %>%
        separate(aname, into = c('well', 'name'), sep = '_') %>%
        left_join(metadata(), by = join_by(well, plate == box_used))
    })
  
  output$cleaned_table <- renderDT({
    zebrabox_data()
    })
  
  output$download_template <- downloadHandler(
    filename = function() { 
      "0000-00-00_zebrabox_experiment_template.xlsx"
    },
    content = function(file) {
      file.copy("0000-00-00_zebrabox_experiment_template.xlsx", file)
    }
  )
  
  output$all_fish_by_minute <- renderPlotly({
    zebrabox_data() %>%
    ggplot(aes(x = time_min, y = activity)) +
      geom_point() +
      labs(title = "Activity vs Time")
    })
  
  output$condition_summary <- renderPlot({ plot.new() })
  
  output$plate_map <- renderPlotly({ 
    zebrabox_data() %>%
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
    })
  
  output$mean_activity <- renderPlot({ 
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity)) %>%
      group_by(treatment, light, well) %>%
      summarize(mean_activity = mean(activity)) %>%
      ungroup() %>%
      filter(light == 'dark') %>%
      ggplot(aes(x = treatment, y = mean_activity)) +
        ggbeeswarm::geom_quasirandom() +
        geom_boxplot(alpha = 0) +
        labs(x = 'Condition', 
             y = 'Mean Activity Dark Cycles',
             title = 'Unnormalized Activity') +
        theme_bw()
    })
  
  output$percent_change <- renderPlot({ plot.new() })
  
  output$activity_by_period <- renderPlot({ plot.new() })
  
  output$percent_change_by_period <- renderPlot({ plot.new() })
  
  output$stats_table <- DT::renderDataTable({
    data.frame(Message = "Statistics will appear here")
  })
  
}

shinyApp(ui, server)








