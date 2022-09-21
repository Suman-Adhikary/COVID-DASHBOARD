library(dplyr)
library(shiny)
library(shinydashboard)
library(lubridate)
library(plotly)
library(rjson)

### Import Data.
dataset <- read.csv(file = "C://Users//suman//Desktop//My//Git//COVID DASHBOARD//Data//Confirmed_cov_state.csv")
Dates <- dataset$Date_YMD
Dates <- dmy(Dates)
States <- colnames(dataset)
Only_state = colSums(select(dataset, "Andaman.and.Nicobar.Islands":"Ladakh"))


### Map building.
state = c("Andaman and Nicobar Islands", "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chandigarh", "Chhattisgarh",
          "Dadra and Nagar Haveli", "Daman and Diu", "Delhi", "Goa", "Gujarat", "Haryana", "Himachal Pradesh","Jammu & Kashmir",
          "Jharkhand", "Karnataka", "Kerala", "Lakshadweep", "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya",
          "Mizoram", "Nagaland", "Odisha", "Puducherry", "Punjab", "Rajasthan", "Sikkim", "Tamil Nadu", "Telangana", "Tripura", "Uttar Pradesh",
          "Uttarakhand", "West Bengal", "Ladakh")
url="https://gist.githubusercontent.com/jbrobst/56c13bbbf9d97d187fea01ca62ea5112/raw/e388c4cae20aa53cb5090210a42ebb9b765c0a36/india_states.geojson"
geojson <- rjson::fromJSON(file=url)



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
              tabBox(id = "T2", width = 6,
                     tabPanel("Confirmed Cases", plotlyOutput("map", height = 380)),
                     tabPanel("Recovered Cases"),
                     tabPanel("Death Cases")),
              tabBox(id = "T3", width = 6,
                    tabPanel("Confirmed Cases", plotlyOutput("lineplot", height = 380)),
                    tabPanel("Recovered Cases"),
                    tabPanel("Death Cases")),
              tabBox(id = "T4", width = 6,
                     tabPanel("Confirmed Cases", plotlyOutput("barplot", height = 380)),
                     tabPanel("Recovered Cases"),
                     tabPanel("Dath Cases")),
              tabBox(id = "T5", width = 6,
                     tabPanel("Confirmed Cases", plotlyOutput("pieplot", height = 380)),
                     tabPanel("Recovered Cases"),
                     tabPanel("Death Cases"))
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
    plot_ly(dataset, labels = ~States[5:41], values =  Only_state) %>%
    add_pie(hole = 0.6)
  })
  
  output$barplot <- renderPlotly({
    plot_ly(dataset, x = ~States[5:41], y = ~Only_state, type = "bar", color = ~Only_state, colors = c("#1B98E0","black"))
  })
  
  output$map <- renderPlotly({
    g <- list(
      fitbounds = "locations",
      visible = TRUE
    )
    fig <- plot_ly() 
    fig <- fig %>% add_trace(
      type="choropleth",
      geojson=geojson,
      locations=state,
      z=Only_state,
      colorscale="Bluered_r",
      featureidkey='properties.ST_NM'
    )
    fig <- fig %>% layout(
      geo = g
    )
    fig
  })
}

shinyApp(ui, server)

