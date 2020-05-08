library(shiny)
library(jsonlite)
library(tidyverse)
library(readxl)
library(Rfast)
source("data/tracker.R")


# Define UI for application that draws a histogram
ui <- navbarPage("NFBC Draft Tracker",
                 
                 
                 tabPanel("My Team",
                          DT::dataTableOutput('myteam')
                 ),
                 tabPanel("Target Tracking",
                          DT::dataTableOutput('targets')
                 ),
                 tabPanel("Big Board",
                          DT::dataTableOutput('bigboard')
                 ),
                 tabPanel("All Available Players",
                          DT::dataTableOutput('available')
                 )
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$myteam <- DT::renderDataTable(DT::datatable({
        data <- my_team}, options = list(paging = FALSE, searching = FALSE),
    rownames= FALSE))
    output$targets <- DT::renderDataTable(DT::datatable({
        data <- target_chart}, options = list(paging = FALSE, searching = FALSE),
        rownames= FALSE))
    output$bigboard <- DT::renderDataTable(DT::datatable({
        data <- big_board}, options = list(paging = TRUE, searching = TRUE),
        rownames= TRUE))
    output$available <- DT::renderDataTable(DT::datatable({
        data <- available}, options = list(paging = TRUE, searching = TRUE),
        rownames= TRUE))
}

# Run the application 
shinyApp(ui = ui, server = server)
