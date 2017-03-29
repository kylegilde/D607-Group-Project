#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# load packages

library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)

# import data, fix column names

df <- my.Skills_tbl
df$Rank <- -df$n %>%
  rank() %>%
  round(0)


# user interface selectes disease and year

ui <- fluidPage(
  headerPanel('Skills by State'),
  sidebarPanel(
    selectInput('skill', 'Skills', unique(df$Skills), selected='Algorithms')
  ),
  mainPanel(
    plotlyOutput('plot1'),
    verbatimTextOutput('mystats')
  )
)

# server code filters and returns data in plotly

server <- function(input, output, session) {
  
  selectedData <- reactive({
    dfSlice <- df %>%
      filter(Skills == input$skill) %>%
      mutate(staterank=min_rank(n)) %>%
      arrange(staterank)
  })
  
  
  output$plot1 <- renderPlotly({
    
    plot_ly(selectedData(), x = ~State, y = ~n, type='bar') %>%
      layout(title = "Number of Skills by State",
             xaxis = list(categoryorder = "array",
                          categoryarray = ~n,
                          type = "category"),
             yaxis = list(title = "Total of Skills"))
  })
  
}

shinyApp(ui = ui, server = server)