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
  )