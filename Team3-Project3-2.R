
# load packages

library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)

# import data, fix column names

df <- my.Salaries_tbl
df$Rank <- -df$Salary %>%
  rank() %>%
  round(0)


# user interface selectes disease and year

ui <- fluidPage(
  headerPanel('Median Salary by State'),
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
      mutate(staterank=min_rank(Salary)) %>%
      arrange(staterank)
  })
  
  
  output$plot1 <- renderPlotly({
    
    plot_ly(selectedData(), x = ~State, y = ~Salary, type='bar') %>%
      layout(title = "Median Salary by State",
             xaxis = list(categoryorder = "array",
                          categoryarray = ~Salary,
                          type = "category"),
             yaxis = list(title = "Median Salary"))
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)