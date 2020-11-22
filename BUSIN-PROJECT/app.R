#loading in a wide variety of packages to ensure smooth sailing 
library(dplyr);library(stringr);library(tidyverse);library(readr);library(ggplot2);library(plotly)
library(lubridate);library(ggthemes);library(RColorBrewer);library(rworldmap);library(mapproj)
library(readxl);library(GGally);library(shiny);library(shinydashboard); library(shinydashboardPlus);
library(naniar)

#loading in data
source("helpers/Script1.R")
  
#title of the dashboard
header <- dashboardHeaderPlus(title = tags$a(tags$img(src="BMW-logo.png", height= '32', width ='32'), tags$b('BMW', style = "color: #222222")),
                              enable_rightsidebar = TRUE,
                              rightSidebarIcon = "gears")
  
  
#Creating the sidebar menu of the dashboard
sidebar <-  dashboardSidebar(
  sidebarMenu(
    id = "nav",
    menuItem("General", tabName = "tab1", icon = icon("dashboard")),
    menuItem("Market Insights", tabName = "tab2", icon = icon("search-plus")),
    menuItem("Market Trends", tabName = "tab3", icon = icon("chart-line")),
    menuItem("Customer Satisfaction", tabName = "tab4", icon = icon("smile"))
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
  
  tags$script('
      $(".navbar-custom-menu").on("click",function(){
        $(window).trigger("resize");
      })'
  ),
  
  tabItems(
    #tab 1
    tabItem(tabName = "tab1",
            fluidPage(
              # infoBoxes with fill=FALSE
                fluidRow(
                # A static infoBox
                  valueBoxOutput("BMW_market_share"),
                  valueBoxOutput("total_sales"),
                  valueBoxOutput("customerSatisfaction")
                  ),
              
                fluidRow(
                  box("Trend Market Share", plotlyOutput("trend_market_share", height = 621), status = "primary"),
                  tabBox(title = "Sales overview", id = "1", 
                         tabPanel("By Segment", plotlyOutput("sales_by_segment", height = 600 )),
                         tabPanel("By model", plotlyOutput("sales_by_model", height = 600))
                         )
                  )
                )
            ),
    #tab 2        
    tabItem(tabName = "tab2",
            fluidPage(
              fluidRow(
                box(selectInput(inputId = "SelectedYear", 
                                label = "Select year", 
                                c("2019","2018"), 
                                selected = "2018",
                                ),
                    status = "primary"
                    ),
                box(radioButtons(inputId = "SelectedMarket", 
                                 label = "Select market",
                                 c("BMW active markets" = "BMW",
                                   "BMW non-active markets" = "NoBMW",
                                   "All" = "All_"),
                                 inline = T),
                    height =100,
                    status = "primary"
                    )
                ),
              fluidRow(
                box("Market share per brand", plotlyOutput("msp_brand", height = 600),
                    status = "primary"),
                box("Sales comparison", plotlyOutput("Sales_comparison", height = 600),
                    status="primary")
                ),
              fluidRow(
                box(),
                tabBox(title = "Model per segment", id = "2", height = 500,
                       tabPanel("Model sales", plotlyOutput("Model_per_segment"),status ="primary"),
                       tabPanel("Settings", selectInput(inputId = "SelectedSegment",
                                            label = "Select Segment",
                                            levels(as.factor(full_segment_sales$type))
                                            ),
                                selectInput(inputId = "SelectedYear2",
                                            label = "Select Year",
                                            c("2019","2020"),
                                            selected = "2020")
              
                                )
                       )
                )
              )
            ),
    #tab 3
    tabItem(tabName = "tab3",
            fluidPage(
              fluidRow(
                box(selectInput(inputId = "selectFuelType",
                                label = "Select Fuel Type",
                                levels(as.factor(colnames(cars_by_fuel_type[4:9]))),
                                selected = "Diesel"),
                    status = "primary"
                ),
                box(selectInput(inputId = "SelectedYear_fuel_type", 
                                label = "Select year", 
                                levels(as.factor(new_cars_by_fuel_type$Year)), 
                                selected = "2018",
                                ),
                status = "primary"
                )),
              fluidRow(
                box("Popularity of fuel types per country", plotlyOutput("fueltype_map", height = 600),
                    status = "primary")
              )
              )
            ),
    #tab 4
    tabItem(tabName = "tab4")
    )
)
# Define UI for application
ui <- dashboardPagePlus(header, sidebar, body, rightsidebar)




# Define server logic required to draw graphs
server <- function(input, output) {
  
#TAB 1: GENERAL------------------------------------------------------------------------------------------------------------------------  
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
  
  output$trend_market_share <- renderPlotly({
    ggplotly(
    annual_sales_bmw %>%
      mutate(Market.Share = as.numeric(Market.Share))%>%
      ggplot(aes(Year,Market.Share)) +
      geom_line() + 
      geom_point() +
      ylab("% market share")+
      if (tail(annual_sales_bmw$Market.Share,1)>input$target_ms) 
        {geom_hline(yintercept = input$target_ms, color = "green")}
    else
      {geom_hline(yintercept = input$target_ms, color = "red")}
      ) 
  })
  
  output$sales_by_segment <- renderPlotly({
    ggplotly(
    full_segment_sales %>%
      filter(str_detect(model,"BMW")) %>%
      group_by(type) %>%
      summarise(Sales_BMW = sum(X2020.H1)) %>%
      mutate(type = fct_reorder(type, Sales_BMW))%>%
      ggplot(aes(type, Sales_BMW)) +
      geom_col() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      xlab("Segment")+
      ylab("Sales")
    )
  })
  
  output$sales_by_model <- renderPlotly({
    ggplotly(
      sales_by_model%>%
        filter(year == max(year))%>%
        group_by(Model)%>%
        summarise(Sales = sum(Sales, na.rm = T))%>%
        mutate(Model = fct_reorder(Model, Sales))%>%
        ggplot(aes(Model,Sales))+
        geom_col() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        xlab("Model")+
        ylab("Sales")
      )
  })
 
#TAB 2: MARKET INSIGHTS----------------------------------------------------------------------------------------------------------------
   
  output$msp_brand <- renderPlotly({
    BMW_value <- sales_by_brand%>%
      filter(Year == input$SelectedYear)%>%
      filter(Brand == "BMW")%>%
      select(Sales)
    
    FilteredSales <- sales_by_brand %>%
      filter(Year == input$SelectedYear)
    
    SumSales <- sales_by_brand%>%
      filter(Year == input$SelectedYear)%>%
      summarise(sum = sum(Sales))
    
    sales_by_brand%>%
      filter(Year == input$SelectedYear)%>%
      plot_ly(labels = ~Brand, values= ~Sales,textfont=list(size=20))%>%
      add_pie(hole = 0.4)%>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             annotations=list(text=paste("BMW: ",round(BMW_value[1,1] / SumSales[1,1] * 100, 2), "%", sep=""), "showarrow"=F, font=list(size = 30, color = "black")))
  })
  
  output$Sales_comparison <- renderPlotly({
    SummarySalesPerSegment <- full_segment_sales %>%
      group_by(type) %>%
      summarise(Sales = sum(X2020.H1))
    
    SummarySalesPerSegmentBMW <- full_segment_sales %>%
      filter(str_detect(model,"BMW")) %>%
      group_by(type) %>%
      summarise(Sales_BMW = sum(X2020.H1))
    
    MarketShareSegments <- SummarySalesPerSegmentBMW %>%
      right_join(SummarySalesPerSegment)
    
    MarketShareSegments[is.na(MarketShareSegments)] <- 0
    
    MarketShareSegments <- MarketShareSegments %>%
      mutate(DeductedSales = Sales-Sales_BMW) %>%
      select(-Sales)
    
    
    filteredMarketShareSegments <- switch(input$SelectedMarket,
                   BMW = MarketShareSegments%>%
                     filter(Sales_BMW>0)%>%
                     mutate(type = fct_reorder(type, -Sales_BMW)),
                   NoBMW = MarketShareSegments%>%
                     filter(Sales_BMW==0)%>%
                     mutate(type = fct_reorder(type, -DeductedSales)),
                   All_ = MarketShareSegments%>%
                     mutate(type = fct_reorder(type, -Sales_BMW)))
    
    
    filteredMarketShareSegments%>%
      plot_ly(x=~type, y=~Sales_BMW, type='bar', name = "Sales BMW", marker = list(color = "#E7222E"))%>%
      add_trace(y=~DeductedSales, name ="Rest Of Market Sales", marker = list(color = "#81C4FF"))%>%
      layout(yaxis = list(title = 'Amount'), barmode = 'stack', legend=list(x=0.7, y=0.9))
  })
  
  output$Model_per_segment <- renderPlotly({ ggplotly(
      full_segment_sales%>%
        gather(Year, Sales, -model, -type)%>%
        filter(type==input$SelectedSegment)%>%
        filter(if(input$SelectedYear2=="2019"){Year == "X2019.H1"} else {Year=="X2020.H1"})%>%
        replace_with_na(replace=list(Sales=0))%>%
        mutate(model = fct_reorder(model, -Sales))%>%
        drop_na(Sales)%>%
        head(10)%>%
        ggplot(aes(model, Sales, fill=factor(ifelse(str_detect(model,"BMW"),"BMW","Others"))))+
        geom_col()+
        theme(axis.text.x = element_text(angle = -25, vjust = 1, hjust=1))+
        scale_fill_manual(name = "model", values=c("#E7222E","#81C4FF"))+
        ggtitle(paste0("Models in ", input$SelectedSegment))+
        theme(legend.position='none')
    , tooltip=c("x","y"))
    
  })
  
#TAB 3: MARKET TRENDS----------------------------------------------------------------------------------------------------------------
  
  output$fueltype_map <- renderPlotly({
    cars_by_fuel_type%>%
      filter(Time == input$SelectedYear_fuel_type)%>%
      select(region, Total, Petroleum_Products, LPG, Diesel, Natural_Gas, Electricity, Alternative_Energy)%>%
      gather(key = "FuelType", value = "amount", -region, -Total)%>%
      filter(FuelType == input$selectFuelType)%>%
      mutate(relative_frequency = round(amount/Total * 100, 2)) -> filtered_fuel_type
    
    europeCoords$value <- filtered_fuel_type$relative_frequency[match(europeCoords$region, filtered_fuel_type$region)]
    
    ggplotly(
      ggplot() + 
        geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value), colour = "black", size = 0.1) +
        coord_map(xlim = c(-13, 35),  ylim = c(32, 71))+
        scale_fill_gradient(name = "relative percentage of cars", low = "#E7222E", high = "#81C4FF", na.value = "grey50")+
        theme_minimal()+
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), axis.title = element_blank(),
              plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
