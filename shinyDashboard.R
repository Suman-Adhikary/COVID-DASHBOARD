library(dplyr)
library(shiny)
library(shinydashboard)
library(lubridate)
library(plotly)

### Import Data.
dataset <- read.csv(file = "C://Users//suman//Desktop//My//Git//COVID DASHBOARD//Data//Confirmed_cov_state.csv")
Dates <- dataset$Date_YMD
Dates <- dmy(Dates)
States <- colnames(dataset)
Only_state = colSums(select(dataset, "Andaman.and.Nicobar.Islands":"West.Bengal"))


### Building UI for dashboard.
ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "COVID INDIA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Data", tabName = "data"),
      menuItem("Dashboard", tabName = "dashboard"),
      selectInput(inputId = "States", label = "Select the States", choices = c(States)[4:40], selected = "Total")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              h2("This is the about.")
      ),
      tabItem(tabName = "data", 
              tabBox(id = "T1", width = 20,
                    tabPanel("About", icon = icon("address-card")),
                    tabPanel("Data Describtion", icon = icon("address-card")),
                    tabPanel("Data", icon = icon("address-card"), dataTableOutput("dataTable")))),
      tabItem(tabName = "dashboard",
              tabBox(id = "T2",
                    tabPanel("Confirmed Cases", plotlyOutput("lineplot", height = 380)),
                    tabPanel("Recovered Cases"),
                    tabPanel("Death Cases")),
              tabBox(id = "T3", width = 6,
                     tabPanel("Confirmed Cases", plotlyOutput("pieplot", height = 380)),
                     tabPanel("Recovered Cases"),
                     tabPanel("Death Cases")),
              tabBox(id = "T4", width = 6,
                     tabPanel("Confirmed Cases", plotlyOutput("barplot", height = 380)),
                     tabPanel("Recovered Cases"),
                     tabPanel("Dath Cases"))
              )
    )
  )
)

### Building plot and visualization.
server <- function(input, output)
{
  output$dataTable <- renderDataTable(
    dataset1
  )
  
  output$lineplot <- renderPlotly(
    plot_ly(dataset, x = ~Dates, y = ~dataset[[input$States]], type = "scatter", mode = "lines") %>%
    layout(title = "CONFIRMED CASES IN INDIA",
                          yaxis = list(title = "Confirmed Cases"),
                          xaxis = list(title = "Year"))
  )
  
  output$pieplot <- renderPlotly({
    plot_ly(dataset, labels = ~States[5:40], values =  Only_state) %>%
    add_pie(hole = 0.6)
  })
  
  output$barplot <- renderPlotly({
    plot_ly(dataset, x = ~States[5:40], y = ~Only_state, type = "bar", color = ~Only_state, colors = c("#1B98E0","black"))
  })
}

shinyApp(ui, server)

