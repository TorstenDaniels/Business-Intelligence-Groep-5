#loading in a wide variety of packages to ensure smooth sailing 
library(dplyr);library(stringr);library(tidyverse);library(readr);library(ggplot2)
library(lubridate);library(ggthemes);library(RColorBrewer);library(rworldmap);library(mapproj)
library(readxl);library(GGally);library(shiny);library(shinydashboard)

#loading in data
source("helpers/Script1.R")
  
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
                valueBoxOutput("BMW_market_share"),
                valueBoxOutput("customerSatisfaction"),
                valueBoxOutput("total_sales")
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
  output$BMW_market_share <- renderValueBox ({
    comparison_sales_market%>%
      tail(1)%>%
      select(BMW) -> BMW_share_last_year
    
    valueBox(paste0(round(BMW_share_last_year * 100, 2), "%"), tagList("BMW Market Share", p(""), "Target Score: 6%"),
             icon = icon("chart-pie"),
             color = "red")
  })
  
  output$customerSatisfaction <- renderValueBox({
    customerSatisfactionBenchark%>%
      filter(year == max(year))%>%
      mutate(averageScore = mean(satisfactionScore, na.rm = T))%>%
      filter(brand == "BMW") -> BMW_customer_satisfaction
    
    valueBox(
      paste0(BMW_customer_satisfaction$satisfactionScore , "%"),
      tagList("Customer Satisfaction", p(""), "Target Scorce: 77%"),
      if (BMW_customer_satisfaction$satisfactionScore > BMW_customer_satisfaction$averageScore) 
            {icon = icon("grin-beam", lib = "font-awesome")}
      else
        {icon = icon("angry", lib = "font-awesome")},
      
      if (BMW_customer_satisfaction$satisfactionScore > BMW_customer_satisfaction$averageScore) 
            {color = "green"}
      else
        {color = "red"}
    )
  })
  
  output$total_sales <- renderValueBox({
    sales_bmw %>%
      tail(2) -> latest_monthly_sales
    c_month <- month(Sys.Date()) #month of the system used for selection in tables
    
    valueBox(latest_monthly_sales[2,c_month+1], #plus one needed to select right column
             tagList(paste0("Monthly Sales ",colnames(sales_bmw[c_month+1])), 
                     p(""), paste0("Monthly sales ",colnames(sales_bmw[c_month+1]), " ", year(Sys.Date())-1, ": ",
                                   latest_monthly_sales[1,(c_month+1)])), 
             icon = icon("car", lib = "font-awesome"),
             if (latest_monthly_sales[2,c_month+1] > latest_monthly_sales[1,c_month+1]) 
                  {color = "green"}
             else
             {color = "red"}
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
