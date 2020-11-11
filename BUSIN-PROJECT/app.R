#loading in a wide variety of packages to ensure smooth sailing 
library(dplyr);library(stringr);library(tidyverse);library(readr);library(ggplot2)
library(lubridate);library(ggthemes);library(RColorBrewer);library(rworldmap);library(mapproj)
library(readxl);library(GGally);library(shiny);library(shinydashboard); library(shinydashboardPlus);

#loading in data
source("helpers/Script1.R")
  
#title of the dashboard
header <- dashboardHeaderPlus(title = "BMW",
                              enable_rightsidebar = TRUE,
                              rightSidebarIcon = "gears")
  
  
#Creating the sidebar menu of the dashboard
sidebar <-  dashboardSidebar(
  sidebarMenu(
    menuItem("General", tabName = "General", icon = icon("dashboard")),
    menuItem("Market Research", tabName = "Market Research"),
    menuItem("Market Trends", tabName = "Market Trends"),
    menuItem("Customer Satisfaction", tabName = "Customer Satisfaction")
  )
)

#creating the rightsidebar of the dashboard
rightsidebar <- rightSidebar(
  background = "dark",
  numericInput(inputId = "target_ms", "Target Market Share %: ", 6),
  sliderInput(inputId = "target_cs", "Targets Customer Satisfaction", min = 0, max = 100, value = 70)
    )


#creating the dashboard body  
body <- dashboardBody(
  #each tab starts from tabItem
  tabItems( 
    tabItem(tabName = "General",
            (fluidPage(
              # infoBoxes with fill=FALSE
                fluidRow(
                # A static infoBox
                  valueBoxOutput("BMW_market_share"),
                  valueBoxOutput("customerSatisfaction"),
                  valueBoxOutput("total_sales")
                  ),
              
                fluidRow(
                  box("Trend Market Share" ,plotOutput("trend_market_share"), status = "primary"),
                  tabBox(title = "Sales overview", id = "1", 
                         tabPanel("By Segment", plotOutput("sales_by_segment")),
                         tabPanel("By model", plotOutput("Sales_by_model"))
                  )
              )
             )
            ),
    tabItem(tabName = "Market Research"),
    tabItem(tabName = "Market Trends"),
    tabItem(tabName = "Customer Satisfaction")
  )
)
)
# Define UI for application
ui <- dashboardPagePlus(header, sidebar,body , rightsidebar)

# Define server logic required to draw graphs
server <- function(input, output) {
  output$BMW_market_share <- renderValueBox ({
    comparison_sales_market%>%
      tail(1)%>%
      select(BMW) -> BMW_share_last_year
    
    valueBox(paste0(round(BMW_share_last_year * 100, 2), "%"), 
             tagList("BMW Market Share", p(""), paste0("Target Share: ", input$target_ms, "%")),
             icon = icon("chart-pie"),
             if (BMW_share_last_year *100 >= input$target_ms)
             {color = "green"}
             else
             {color = "red"})
  })
  
  output$customerSatisfaction <- renderValueBox({
    customerSatisfactionBenchark%>%
      filter(year == max(year))%>%
      mutate(averageScore = mean(satisfactionScore, na.rm = T))%>%
      filter(brand == "BMW") -> BMW_customer_satisfaction
    
    valueBox(
      paste0(BMW_customer_satisfaction$satisfactionScore , "%"),
      tagList("Customer Satisfaction", p(""), paste0("Target Score: ", input$target_cs, "%")),
      if (BMW_customer_satisfaction$satisfactionScore > input$target_cs) 
            {icon = icon("grin-beam", lib = "font-awesome")}
      else
        {icon = icon("angry", lib = "font-awesome")},
      
      if (BMW_customer_satisfaction$satisfactionScore > input$target_cs) 
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
  
  output$trend_market_share <- renderPlot({
    annual_sales_bmw %>%
      ggplot(aes(Year,as.numeric(Market.Share))) +
      geom_line() + geom_hline(yintercept = input$target_ms, color = "red") + ylab("% market share")
  })
  
  output$sales_by_segment <- renderPlot({
    full_segment_sales %>%
      filter(str_detect(model,"BMW")) %>%
      group_by(type) %>%
      summarise(Sales_BMW = sum(X2020.H1)) %>%
      ggplot(aes(reorder(type, Sales_BMW), Sales_BMW)) +
      geom_col() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      xlab("Segment")
  })
  
  output$sales_by_model <- renderPlot({
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
