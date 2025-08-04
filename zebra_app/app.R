#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# ZEBRA APP

library(shiny)
library(bslib)
library(vroom)
library(broom)
library(dplyr)
library(ggplot2)
library(stringr)
library(writexl)
library(DT)
library(plotly)
library(tidyr)
library(tibble)
library(ggbeeswarm)
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
            nav_panel("Percent Change Plot", plotlyOutput("percent_change_plot")),
            nav_panel("Percent Change Table", DTOutput("percent_change_table")),
            nav_panel("Percent Change by Period Plot", plotlyOutput("percent_change_by_period_plot")),
            nav_panel("Percent Change by Period Table", DTOutput ("percent_change_by_period_table")),
            nav_panel("Activity by Period", plotlyOutput("activity_by_period")),
            nav_panel("Statistical Summary", DTOutput("stats_table")),
            nav_panel("Cleaned Data", DTOutput("cleaned_table")),
            nav_panel("Peak Height Table", DTOutput("peak_height_table")),
            nav_panel("Peak Height Plot", plotlyOutput("peak_height_plot")),
            nav_panel("Pre-Peak Activity Slope", plotlyOutput("starting_slope_plot")),
            nav_panel("Post-Peak Activity Slope", plotlyOutput("ending_slope_plot"))
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
  
  control_group <- "WT"
  
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
    req(zebrabox_data())
    data <- zebrabox_data()
    
    qc_all_act_max <- data %>%
      filter(startreason == 'Beginning of period', !is.na(activity)) %>%
      filter(ifelse(str_detect(plate, 'LocB'), plate == 'LocB', plate == 'LocA')) %>%
      summarize(max_act = max(activity, na.rm = TRUE)) %>%
      pull(max_act)
    
    plot <- data %>%
      filter(startreason == 'Beginning of period') %>%
      ggplot(aes(x = time_min, y = activity, color = well, group = well)) +
      geom_rect(aes(fill = light, xmin = time_min,
                    xmax = time_min + 1, ymin = 0, ymax = qc_all_act_max + 100),
                color = NA) +
      scale_fill_manual(values = c('gray80', 'white')) +
      geom_vline(xintercept = seq(0, max(data$time_min), 10),
                 linetype = 'dashed', color = 'gray60') +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(0, max(data$time_min), 10)) +
      facet_wrap(~ plate, ncol = 1) +
      coord_cartesian(ylim = c(0, qc_all_act_max + 100)) +
      labs(x = 'Time (min)', y = 'Activity', title = 'Zebrabox Individual Fish') +
      theme_classic(base_size = 16)
  })
  
  
  output$condition_summary <- renderPlotly({ 
    req(zebrabox_data())
    data <- zebrabox_data()
    
    qc_condition_summary <- data %>%
      filter(startreason == 'Beginning of period') %>% 
      group_by(plate, treatment, light, time_min) %>%
      summarize(mean_activity = mean(activity, na.rm = T)) %>%
      ungroup() 
    
    qc_condition_max <- qc_condition_summary %>%
      filter(ifelse(str_detect(plate, 'LocB'), plate == 'LocB', plate == 'LocA')) %>%
      filter(mean_activity == max(mean_activity)) %>%
      select(mean_activity) %>%
      deframe()
    
    ### plot an interactive graph of summarized activity by treatment
    zebrabox_summarized <- qc_condition_summary %>%
      ggplot(aes(x = time_min, y = mean_activity, color = treatment, group = treatment)) +
      geom_rect(aes(fill = light, xmin = time_min,
                    xmax = time_min + 1, ymin = 0, ymax = qc_condition_max + 100), color = NA) +
      scale_fill_manual(values = c('gray80', 'white')) +
      geom_vline(xintercept = seq(0, max(data$time_min), 10), linetype = 'dashed', color = 'gray60') +
      geom_point() +
      geom_line() +
      facet_wrap(~ plate) +
      coord_cartesian(ylim = c(0, qc_condition_max)) +
      labs(x = 'Time (min)', y = 'Activity', title = 'Zebrabox Summarized Conditions') +
      theme_classic(base_size = 16) 
  })
  
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
  
  output$percent_change_by_period_plot <- renderPlotly({
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity),
             treatment == control_group) %>%
      mutate(period3 = ifelse(period2 %in% 1:2, 
                              'acclimation', as.character(period2)),
             period3 = factor(period3, levels = c('acclimation', '3', '4', '5', 
                                                  '6', '7', '8', '9', '10'))) %>% 
      group_by(period3) %>%
      summarize(unique_name = median(activity)) %>%
      ungroup() -> median_cycle_activity
    
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity)) %>%
      mutate(period3 = ifelse(period2 %in% 1:2, 
                              'acclimation', as.character(period2)),
             period3 = factor(period3, levels = c('acclimation', '3', '4', '5', 
                                                  '6', '7', '8', '9', '10'))) %>%
      left_join(median_cycle_activity, by = join_by(period3)) %>%
      mutate(percent_change = ((activity - unique_name) / unique_name) * 100) %>%
      group_by(period3, treatment, light, well) %>%
      summarize(mean_activity = mean(percent_change)) %>%
      ungroup() %>%
      ggplot(aes(x = period3, y = mean_activity, color = treatment)) +
      annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf, fill = 'gray80') +
      annotate("rect", xmin = 3.5, xmax = 4.5, ymin = -Inf, ymax = Inf, fill = 'gray80') +
      annotate("rect", xmin = 5.5, xmax = 6.5, ymin = -Inf, ymax = Inf, fill = 'gray80') +
      annotate("rect", xmin = 7.5, xmax = 8.5, ymin = -Inf, ymax = Inf, fill = 'gray80') +
      geom_vline(xintercept = seq(1.5, 8.5, 1), linetype = 'dashed', color = 'gray60') +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'gray60') +
      geom_boxplot(alpha = 0) +
      geom_point(position = position_dodge(width = .75)) +
      coord_cartesian(ylim = c(-200, 200)) +
      labs(x = 'Cycle', 
           y = 'Percent Change From Control Median Activity',
           title = 'Percent Change') +
      theme_bw()
  })
  
  
  output$percent_change_by_period_table <- renderDT({
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity),
             treatment == control_group) %>%
      mutate(period3 = ifelse(period2 %in% 1:2, 
                              'acclimation', as.character(period2)),
             period3 = factor(period3, levels = c('acclimation', '3', '4', '5', 
                                                  '6', '7', '8', '9', '10'))) %>% 
      group_by(period3) %>%
      summarize(unique_name = median(activity)) %>%
      ungroup() -> median_cycle_activity
    
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity)) %>%
      mutate(period3 = ifelse(period2 %in% 1:2, 
                              'acclimation', as.character(period2)),
             period3 = factor(period3, levels = c('acclimation', '3', '4', '5', 
                                                  '6', '7', '8', '9', '10'))) %>%
      left_join(median_cycle_activity, by = join_by(period3)) %>%
      mutate(percent_change = ((activity - unique_name) / unique_name) * 100) %>%
      group_by(period3, treatment, light, well) %>%
      summarize(mean_activity = mean(percent_change)) %>%
      ungroup() %>%
      rename(period = period3, percent_change = mean_activity) %>%
      DT::datatable(extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   buttons = c('copy', 'csv', 'excel'),
                                   lengthMenu = list(c(10, 25, 50, -1),
                                                     c(10, 25, 50, "All"))))
  })
  
  output$activity_by_period <- renderPlotly({ 
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity)) %>%
      mutate(period3 = ifelse(period2 %in% 1:2, 
                              'acclimation', as.character(period2)),
             period3 = factor(period3, levels = c('acclimation', '3', '4', '5', 
                                                  '6', '7', '8', '9', '10'))) %>%
      group_by(period3, treatment, light, well) %>%
      summarize(mean_activity = mean(activity)) %>%
      ungroup() %>%
      ggplot(aes(x = period3, y = mean_activity, color = treatment)) +
      annotate("rect", xmin = 1.5, xmax = 2.5, ymin = 0, ymax = Inf, fill = 'gray80') +
      annotate("rect", xmin = 3.5, xmax = 4.5, ymin = 0, ymax = Inf, fill = 'gray80') +
      annotate("rect", xmin = 5.5, xmax = 6.5, ymin = 0, ymax = Inf, fill = 'gray80') +
      annotate("rect", xmin = 7.5, xmax = 8.5, ymin = 0, ymax = Inf, fill = 'gray80') +
      geom_vline(xintercept = seq(1.5, 8.5, 1), linetype = 'dashed', color = 'gray60') +
      geom_boxplot(alpha = 0) +
      # ggbeeswarm::geom_quasirandom(position = position_dodge(width = .75)) +
      geom_point(position = position_dodge(width = .75)) +
      labs(x = 'Cycle', 
           y = 'Mean by Dark Cycle',
           title = 'Unnormalized Activity') +
      theme_bw()
  })
  
  output$percent_change_plot <- renderPlotly({
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity),
             strain == control_group, light == 'dark') %>%
      summarize(median(activity)) %>%
      deframe() -> median_activity
    
    
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity)) %>%
      mutate(percent_change = ((activity - median_activity) / median_activity) * 100) %>%
      group_by(treatment, light, well) %>%
      summarize(mean_activity = mean(percent_change)) %>%
      ungroup() %>%
      filter(light == 'dark') %>%
      ggplot(aes(x = treatment, y = mean_activity)) +
      ggbeeswarm::geom_quasirandom() +
      geom_boxplot(alpha = 0) +
      labs(x = 'Condition', 
           y = 'Average Percent Change From Control Median\nin Dark Cycles',
           title = 'Percent Change Activity') +
      theme_bw()
  })
  
  output$percent_change_table <- renderDT({
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity),
             strain == control_group, light == 'dark') %>%
      summarize(median(activity)) %>%
      deframe() -> median_activity
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity)) %>%
      mutate(percent_change = ((activity - median_activity) / median_activity) * 100) %>%
      group_by(treatment, light, well) %>%
      summarize(mean_activity = mean(percent_change)) %>%
      ungroup() %>%
      filter(light == 'dark') %>%
      rename(mean_percent_change = mean_activity) %>%
      DT::datatable(extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   buttons = c('copy', 'csv', 'excel'),
                                   lengthMenu = list(c(10, 25, 50, -1),
                                                     c(10, 25, 50, "All"))))
  })
  
  output$stats_table <- DT::renderDataTable({
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity)) %>%
      mutate(period3 = ifelse(period2 %in% 1:2, 
                              'acclimation', as.character(period2)),
             period3 = factor(period3, levels = c('acclimation', '3', '4', '5', 
                                                  '6', '7', '8', '9'))) %>%
      group_by(period3, treatment, light, well) %>%
      summarize(mean_activity = mean(activity)) %>%
      ungroup() %>%
      filter(light == 'dark') %>%
      group_by(period3) %>%
      nest() %>%
      ungroup() %>%
      mutate(test = map(data, ~ TukeyHSD(aov(mean_activity ~ treatment, data = .))),
             class = map(test, ~ class(.))) %>%
      unnest(c(class)) %>%
      filter(class != 'try-error') %>%
      mutate(test = map(test, ~ broom::tidy(.))) %>%
      unnest(c(test)) %>%
      select(period = period3, 
             groups_tested = contrast,
             difference_means = estimate,
             conf.low, conf.high, adj.p.value) %>%
      mutate(difference_means = round(difference_means),
             conf.low = round(conf.low),
             conf.high = round(conf.high),
             adj.p.value = round(adj.p.value, 4)) %>%
      mutate(significant = ifelse(adj.p.value < 0.05, 'significant', 'not significant')) %>%
      DT::datatable(extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   buttons = c('copy', 'csv', 'excel'),
                                   lengthMenu = list(c(10, 25, 50, -1),
                                                     c(10, 25, 50, "All"))))
  })
  
  
  output$peak_height_table <- DT::renderDataTable({
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity)) %>%
      group_by(treatment, light, period2, well) %>%
      mutate(index = row_number()) %>%
      nest() %>%
      ungroup() %>%
      mutate(period_peak = map(data, ~ as_tibble(pracma::findpeaks(.$activity, npeaks = 1)))) %>%
      unnest(c(period_peak)) %>%
      rename(peak_height = V1, peak_index = V2, curve_start_index = V3,
             curve_end_index = V4) %>%
      unnest(c(data)) %>%
      mutate(max_peak = ifelse(index == peak_index, 'max', NA_character_)) -> zebrabox_data_w_peaks
    
    # test with linear model
    zebrabox_data_w_peaks %>%
      filter(max_peak == 'max') %>%
      distinct(treatment, period2, light, well, peak_height) %>%
      filter(light == 'dark') %>%
      group_by(treatment) %>%
      nest() %>%
      ungroup() %>%
      mutate(test = map(data, ~ broom::tidy(lm(peak_height ~ period2, data = .)))) %>%
      unnest(c(test)) %>%
      filter(term != '(Intercept)') %>%
      select(treatment, peak_height_change_over_time = estimate,
             pvalue = p.value) %>%
      mutate(peak_height_change_over_time = round(peak_height_change_over_time, 2),
             pvalue = round(peak_height_change_over_time, 2)) %>%
      DT::datatable(extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   buttons = c('copy', 'csv', 'excel'),
                                   lengthMenu = list(c(10, 25, 50, -1),
                                                     c(10, 25, 50, "All"))),
                    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; color:black;  font-size:200% ;',
                                                      'Zebrabox'))
  })
  output$peak_height_plot <- renderPlotly({
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity)) %>%
      group_by(treatment, light, period2, well) %>%
      mutate(index = row_number()) %>%
      nest() %>%
      ungroup() %>%
      mutate(period_peak = map(data, ~ as_tibble(pracma::findpeaks(.$activity, npeaks = 1)))) %>%
      unnest(c(period_peak)) %>%
      rename(peak_height = V1, peak_index = V2, curve_start_index = V3,
             curve_end_index = V4) %>%
      unnest(c(data)) %>%
      mutate(max_peak = ifelse(index == peak_index, 'max', NA_character_)) -> zebrabox_data_w_peaks
    
    zebrabox_data_w_peaks %>%
      filter(max_peak == 'max') %>%
      distinct(treatment, period2, light, well, peak_height) %>%
      filter(light == 'dark') %>%
      ggplot(aes(x = period2, y = peak_height, color = treatment, group = well)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = c(3, 5, 7, 9)) +
      labs(x = 'Condition',
           y = 'Max Curve Height Dark Cycles',
           title = 'Zebrabox') +
      facet_wrap(~ treatment) +
      theme_bw()
  })
  
  output$starting_slope_plot <- renderPlotly({
    
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity)) %>%
      group_by(treatment, light, period2, well) %>%
      mutate(index = row_number()) %>%
      nest() %>%
      ungroup() %>%
      mutate(period_peak = map(data, ~ as_tibble(pracma::findpeaks(.$activity, npeaks = 1)))) %>%
      unnest(c(period_peak)) %>%
      rename(peak_height = V1, peak_index = V2, curve_start_index = V3,
             curve_end_index = V4) %>%
      unnest(c(data)) %>%
      mutate(max_peak = ifelse(index == peak_index, 'max', NA_character_)) -> zebrabox_data_w_peaks
    
    zebrabox_data_w_peaks %>% #distinct(period) %>% mutate(period2 = str_remove(period, '^0+:')
      group_by(treatment, well, light, period2) %>%
      filter(index <= peak_index) %>% #filter(well == 'c1-004') %>% tail(1) %>% nest() %>% ungroup() %>% mutate(test = map(data, ~ .$activity)) %>% unnest(c(test))
      nest() %>%
      ungroup() %>%
      mutate(len = map(data, ~nrow(.))) %>%
      unnest(c(len)) %>%
      mutate(period_start_slope = case_when(len == 1 ~ map(data, ~ .$activity), # maybe return 0 or NA?
                                            len == 2 ~ map(data, ~ as.numeric(dist(select(.,
                                                                                          activity, time_min),
                                                                                   method = 'euclidean'))),
                                            TRUE ~ map(data, ~ filter(broom::tidy(lm(activity ~ time_min, data = .)),
                                                                      term == 'time_min')$estimate))) %>%
      unnest(c(period_start_slope)) %>%
      distinct(treatment, period2, light, well, period_start_slope) %>%
      filter(light == 'dark') %>%
      ggplot(aes(x = period2, y = period_start_slope, color = treatment, group = well)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = c(3, 5, 7, 9)) +
      labs(x = 'Condition',
           y = 'Starting Slope Dark Cycles',
           title = 'Zebrabox') +
      facet_wrap(~ treatment) +
      theme_bw()
  })
  output$ending_slope_plot <- renderPlotly({
    
    zebrabox_data() %>%
      filter(datatype == 'QuantizationSum', !is.na(activity)) %>%
      group_by(treatment, light, period2, well) %>%
      mutate(index = row_number()) %>%
      nest() %>%
      ungroup() %>%
      mutate(period_peak = map(data, ~ as_tibble(pracma::findpeaks(.$activity, npeaks = 1)))) %>%
      unnest(c(period_peak)) %>%
      rename(peak_height = V1, peak_index = V2, curve_start_index = V3,
             curve_end_index = V4) %>%
      unnest(c(data)) %>%
      mutate(max_peak = ifelse(index == peak_index, 'max', NA_character_)) -> zebrabox_data_w_peaks
    
    zebrabox_data_w_peaks %>%
      group_by(treatment, well, light, period2) %>%
      filter(index >= peak_index) %>% #filter(well == 'c1-004') %>% tail(1) %>% nest() %>% ungroup() %>% mutate(test = map(data, ~ .$activity)) %>% unnest(c(test))
      nest() %>%
      ungroup() %>%
      mutate(len = map(data, ~nrow(.))) %>%
      unnest(c(len)) %>%
      mutate(period_start_slope = case_when(len == 1 ~ map(data, ~ .$activity),
                                            len == 2 ~ map(data, ~ as.numeric(dist(select(.,
                                                                                          activity, time_min),
                                                                                   method = 'euclidean'))),
                                            TRUE ~ map(data, ~ filter(broom::tidy(lm(activity ~ time_min, data = .)),
                                                                      term == 'time_min')$estimate))) %>%
      unnest(c(period_start_slope)) %>%
      distinct(treatment, period2, light, well, period_start_slope) %>%
      filter(light == 'dark') %>%
      ggplot(aes(x = period2, y = period_start_slope, color = treatment, group = well)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = c(3, 5, 7, 9)) +
      labs(x = 'Condition',
           y = 'Ending Slope Dark Cycles',
           title = 'Zebrabox') +
      facet_wrap(~ treatment) +
      theme_bw()
  })
}


shinyApp(ui, server)










