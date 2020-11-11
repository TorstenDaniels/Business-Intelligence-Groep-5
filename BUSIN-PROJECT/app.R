#loading in a wide variety of packages to ensure smooth sailing 
library(dplyr);library(stringr);library(tidyverse);library(readr);library(ggplot2)
library(lubridate);library(ggthemes);library(RColorBrewer);library(rworldmap);library(mapproj)
library(readxl);library(GGally);library(shiny);library(shinydashboard)


  
#title of the dashboard
header <- dashboardHeader(title = "BMW")
  
  
#Creating the sidebar menu of the dashboard
sidebar <-  dashboardSidebar(
  sidebarMenu(
    menuItem("General", tabName = "General", icon = icon("dashboard")),
    menuItem("Market Research", tabName = "Market Research"),
    menuItem("Market Trends", tabName = "Market Trends"),
    menuItem("Customer Satisfaction", tabName = "Customer Satisfaction")
  )
)


#creating the dashboard body  
body <- dashboardBody(
  #each tab starts from tabItem
  tabItems( 
    tabItem(tabName = "General",
            (
              # infoBoxes with fill=FALSE
              fluidRow(
                # A static infoBox
                infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                # Dynamic infoBoxes
                infoBoxOutput("progressBox"),
                infoBoxOutput("approvalBox")
                )
              )
            ),
    tabItem(tabName = "Market Research"),
    tabItem(tabName = "Market Trends"),
    tabItem(tabName = "Customer Satisfaction")
  )
)

# Define UI for application
ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw graphs
server <- function(input, output) {
    output$progressBox <- renderInfoBox({
      infoBox(
        "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
        color = "purple"
      )
    })
    
    output$approvalBox <- renderInfoBox({
      infoBox(
        "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
        color = "yellow"
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
