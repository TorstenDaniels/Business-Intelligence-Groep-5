#loading in a wide variety of packages to ensure smooth sailing 
library(dplyr);library(stringr);library(tidyverse);library(readr);library(ggplot2);library(plotly)
library(lubridate);library(ggthemes);library(RColorBrewer);library(rworldmap);library(mapproj)
library(readxl);library(GGally);library(shiny);library(shinydashboard); library(shinydashboardPlus);
library(naniar);library(ggimage); library(gghighlight);library(gtrendsR)

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
  numericInput(inputId = "target_sm", "Target Sales this month: ", 
               sales_bmw%>%filter(sales_bmw$Year == year(Sys.Date())-1)%>%select(month(Sys.Date())+1)),
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
                  tabBox(title = "Market overview", #verander deze naam gerust
                         tabPanel("Trend Market Share", plotlyOutput("trend_market_share", height = 600)),
                         tabPanel("Trend Stock Price BMW Group", plotlyOutput("trend_stock_price", height = 500),
                                  numericInput(inputId = "target_sp", "Target Stock Price \u20ac: ", max(BMW_Stock$Close)))
                         ),
                  
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
                tabBox(title = "Brand market shares", height = 600,
                       tabPanel("Market share per brand", plotlyOutput("msp_brand", height = 500)),
                       tabPanel("Settings", selectInput(inputId = "SelectedYear", 
                                                        label = "Select year", 
                                                        c("2019","2018"), 
                                                        selected = "2018")
                                )
                       ),
                
                tabBox(title = "Sales comparison", height = 600,
                       tabPanel("Sales comparison", plotlyOutput("Sales_comparison", height = 500)),
                       tabPanel("settings", radioButtons(inputId = "SelectedMarket", 
                                                         label = "Select market",
                                                         c("BMW active markets" = "BMW",
                                                           "BMW non-active markets" = "NoBMW",
                                                           "All" = "All_"),
                                                         inline = T)
                                )
                       )
                ),
              
              fluidRow(
                tabBox(title = "BCG-matrix BMW models", id="2", height = 600,
                       tabPanel("BCG-matrix", plotOutput("bcg_bmw", height = 450)),
                       tabPanel("Settings", checkboxInput(inputId = "outlier",
                                                          label = "Show electric vehicles",
                                                          value = F))
                       ),
                
                tabBox(title = "Model sales per segment", id = "3", height = 600,
                       tabPanel("Model sales", plotlyOutput("Model_per_segment", height = 500),status ="primary"),
                       tabPanel("Settings", selectInput(inputId = "SelectedSegment",
                                            label = "Select Segment",
                                            selected = "large_car",
                                            levels(as.factor(full_segment_sales$type))
                                            ),
                                selectInput(inputId = "SelectedYear2",
                                            label = "Select Year",
                                            levels(as.factor(full_segment_sales$year)))
              
                                )
                       )
                )
              )
            ),
    #tab 3
    tabItem(tabName = "tab3",
            fluidPage(
              fluidRow(
                tabBox(title = "Fueltype trends", id = "4", height = 600,
                       tabPanel("Fuel type map", plotlyOutput("fueltype_map", height = 450),
                                sliderInput(inputId = "SelectedYear_fuel_type",
                                            label = "Select Year",
                                            min = min(new_cars_by_fuel_type$Year),
                                            max = max(new_cars_by_fuel_type$Year),
                                            step = 1,
                                            value = min(new_cars_by_fuel_type$Year),
                                            animate = animationOptions(interval = 5000, loop = TRUE))
                                ),
                       
                       tabPanel("Settings", 
                                selectInput(inputId = "selectFuelType",
                                            label = "Select Fuel Type",
                                            levels(as.factor(colnames(cars_by_fuel_type[4:9]))),
                                            selected = "Diesel")
                                )
                       ),
                tabBox(title = "Sales trends", id = "4", height = 600,
                       tabPanel("BMW Group Sales", plotlyOutput("bmw_group_sales", height = 500),status = "primary"),
                       tabPanel("BMW Brand Sales", plotlyOutput("bmw_brand_sales", height = 500),status = "primary"),
                       tabPanel("BMW Model Sales", plotlyOutput("bmw_model_sales", height = 450),
                                selectizeInput(inputId = "selectModel",
                                            label = "Select Model",
                                            multiple = T,
                                            selected = "BMW 1-series",
                                            options = list(maxItems = 5 ),
                                            levels(as.factor(sales_by_model$Model)),
                                            width = 600)
                                ,status = "primary")
                       )
                ),
              fluidRow(
                tabBox(title = "Segment trends", id = "5", height = 650,
                       tabPanel("Distribution segments over years", plotlyOutput("distribution_segments", height = 600),status ="primary")
                       # ,tabPanel("Settings")
                       ),
                tabBox(title = "Google trends", id = "6", height = 600,
                       tabPanel("Keyword trends", plotlyOutput("google_trends"),status ="primary"),
                       tabPanel("Settings",
                                textInput(inputId = 'GT_Terms',
                                          label = "Input one or more terms. Use commma to seperate terms",
                                          value = "BMW, Audi, Tesla"),
                                selectInput(inputId = "GT_Time", 
                                            label = "Select the timeframe you want to compare",
                                            choices = c("Last hour" = "now 1-H", 
                                                        "Last four hours" = "now 4-H",
                                                        "Last day" = "now 1-d",
                                                        "Last seven days" = "now 7-d",
                                                        "Last 30 days" = "today 1-m",
                                                        "Last 3 months" = "today 3-m", 
                                                        "Last 12 months" = "today 12-m", 
                                                        "Last five years" = "today+5-y",
                                                        "All (2004 - now)" = "all"),
                                            selected = "now 1-H")
                                )
                       )
                )
              )
            ),
    #tab 4
    tabItem(tabName = "tab4",
            fluidPage(
              fluidRow(
                box(selectizeInput(inputId = "selectBrand",
                              label = "Select Brands",
                              choices = intersect(satisfaction_per_brand$Brand, customerSatisfactionBenchark$brand),
                              selected = "BMW",
                              options = list(maxItems = 3),
                              ),
                    status = "primary"
                  ),
                box(selectizeInput(inputId = "selectSatisfactionType",
                                   label = "Select Satisfaction Type",
                                   choices = levels(as.factor(satisfaction_per_brand$type)),
                                   selected = "Driving experience",
                                   options = list(maxItems = 3)),
                    status = "primary")
                ),
              fluidRow(
                box("Overall customer satisfaction", plotlyOutput("overallCustomerSat"),
                    status = "primary"),
                box("Detailed customer satisfaction", plotlyOutput("detailedCustomerSat"),
                    status = "primary")
                ),
              fluidRow(
                box("Customer loyalty", plotlyOutput("customer_loyalty"),
                    status = "primary")
                )
              )
            )
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
      filter(Year== c(year(Sys.Date()), year(Sys.Date()) - 1)) -> latest_monthly_sales
    
    valueBox(latest_monthly_sales[2,month(Sys.Date())+1], #plus one needed to select right column
             tagList(paste0("Monthly Sales ",colnames(sales_bmw[month(Sys.Date())+1])), 
                     p(""), paste0("Sales target ",colnames(sales_bmw[month(Sys.Date())+1]), ": ",
                                   input$target_sm)), 
             icon = icon("car", lib = "font-awesome"),
             if (latest_monthly_sales[2,month(Sys.Date())+1] > input$target_sm) 
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
      theme_minimal() +
      ylab("% market share")+
      scale_x_continuous(breaks=scales::pretty_breaks(n = 8))+
      if (tail(annual_sales_bmw$Market.Share,1)>input$target_ms) 
        {geom_hline(yintercept = input$target_ms, color = "green")}
      else
        {geom_hline(yintercept = input$target_ms, color = "red")}
      ) 
  })
  
  output$trend_stock_price <- renderPlotly({
    ggplotly(
      BMW_Stock%>%
        ggplot(aes(ymd(Date), Close))+
        geom_line()+
        theme_minimal()+
        xlab("Date") +
        ylab("Stock Price in \u20ac")+
        if (tail(BMW_Stock$Close ,1) > input$target_sp) 
          {geom_hline(yintercept = input$target_sp, color = "green")}
        else
          {geom_hline(yintercept = input$target_sp, color = "red")}
    )
  })
  
  output$sales_by_segment <- renderPlotly({
    ggplotly(
      full_segment_sales %>%
        spread(year, sales) %>%
        filter(str_detect(model,"BMW")) %>%
        group_by(type) %>%
        summarise(Sales_BMW_2018 = sum(`2018`), Sales_BMW_2019 = sum(`2019`), Sales_BMW_2020 = sum(`2020`)) %>%
        mutate(hit_target = ifelse(Sales_BMW_2020 > Sales_BMW_2019, "Hit", "No hit")) %>%
        mutate(type = fct_reorder(type, Sales_BMW_2020))%>%
        ggplot() +
        geom_col(aes(type, Sales_BMW_2020, fill = hit_target)) +
        theme_minimal() +
        scale_fill_manual(values = c("firebrick2", "forestgreen")) + #kleur is afhankelijk van aflabetische volgorde!!! iemand opls?
        geom_col(aes(type, Sales_BMW_2019), fill = NA, colour = "#81C4FF")+
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        xlab("")+
        ylab("Sales")
    ) %>% layout(showlegend = F)
  })
  
  output$sales_by_model <- renderPlotly({
    ggplotly(
      full_segment_sales %>%
        filter(year == max(year)) %>%
        filter(str_detect(model, 'BMW'))%>%
        mutate(model = fct_reorder(model, sales))%>%
        ggplot(aes(model, sales, fill = type))+
        geom_col() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        xlab("")+
        ylab("Sales")
      # 
      # sales_by_model%>%
      #   filter(year == max(year))%>%
      #   group_by(Model)%>%
      #   summarise(Sales = sum(Sales, na.rm = T))%>%
      #   mutate(Model = fct_reorder(Model, Sales))%>%
      #   ggplot(aes(Model,Sales))+
      #   geom_col() +
      #   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      #   xlab("")+
      #   ylab("Sales")
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
             annotations=list(text=paste("BMW: ",round(BMW_value[1,1] / SumSales[1,1] * 100, 2), "%", sep=""), "showarrow"=F, font=list(size = 25, color = "black")))
  })
  
  output$Sales_comparison <- renderPlotly({
    SummarySalesPerSegment <- full_segment_sales %>%
      filter(year == 2020)%>% #select input possible !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      group_by(type) %>%
      summarise(Sales = sum(sales, na.rm = T))
    
    SummarySalesPerSegmentBMW <- full_segment_sales %>%
      filter(year == 2020)%>% #select input possible !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      filter(str_detect(model,"BMW")) %>%
      group_by(type) %>%
      summarise(Sales_BMW = sum(sales, na.rm = T))
    
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
                     mutate(type = fct_reorder(type, -DeductedSales)))
    
    
    filteredMarketShareSegments%>%
      plot_ly(x=~type, y=~Sales_BMW, type='bar', name = "Sales BMW", marker = list(color = "#E7222E"))%>%
      add_trace(y=~DeductedSales, name ="Rest Of Market Sales", marker = list(color = "#81C4FF"))%>%
      layout(yaxis = list(title = 'Sales'), barmode = 'stack', legend=list(x=0.7, y=0.9),
             xaxis = list(title = ""))
  })
  
  
  
  output$bcg_bmw <- renderPlot({
    market_growth <- full_segment_sales %>%
      filter(year == 2019 | year == 2018) %>%
      group_by(type, year) %>%
      summarise(total_sales = sum(sales)) %>%
      spread(year, total_sales) %>%
      summarise(market_growth = round(((`2019`-`2018`)/`2018`)*100,2))
    
    tot_sales_segment <- full_segment_sales %>%
      filter(year == 2019) %>%
      group_by(type) %>%
      summarise(total_sales = sum(sales))
    
    bmw_sales_segment <- full_segment_sales %>%
      filter(year == 2019) %>%
      filter(str_detect(model, 'BMW')) %>%
      summarise(model, sales, type)
    
    bcg_dataset <- left_join(bmw_sales_segment, tot_sales_segment)
    bcg_dataset <- left_join(bcg_dataset, market_growth) %>%
      mutate(market_share = round((sales/total_sales)*100, 2))%>%
      mutate(Image = c("pictures/1series.png", "pictures/2-seriesconvertible.png","pictures/i3.png","pictures/5series.png",
                       "pictures/6series.png","pictures/x5.png","pictures/x6.png","pictures/x7.png","pictures/3series.png","pictures/4series.png",
                       "pictures/x3.png","pictures/x4.png","pictures/2-active.png","pictures/7series.png","pictures/8series.png"))%>%
      mutate(width=80, height=100)
    
    if(!input$outlier){
      bcg_dataset_filtered <- bcg_dataset %>% filter(type != "electric_vehicle")
    }
    else{
      bcg_dataset_filtered <- bcg_dataset
    }
    
    mean_market_share <- bcg_dataset_filtered %>% summarise(median(market_share))
    mean_market_growth <- bcg_dataset_filtered %>% summarise(median(market_growth))
    
    
     # ggplotly(
        bcg_dataset_filtered %>%
          ggplot(aes(market_share, market_growth))+
          geom_image(aes(image=Image),size=0.13, asp = 2) +
          geom_hline(yintercept= 4.5, linetype="dashed", color = "red") +
          geom_vline(xintercept= mean_market_share[1,1], linetype="dashed", color = "red")+
          theme_minimal()
         # ,tooltip= c("text", "x", "y", "size")
         # )
  })
  
  
  output$Model_per_segment <- renderPlotly({
    full_segment_sales%>%
      filter(type==input$SelectedSegment)%>%
      filter(year== input$SelectedYear2)%>%
      mutate(model = fct_reorder(model, -sales))%>%
      head(10) -> models_per_segment
    
    contains_bmw_boolean = F
    for (i in 1:nrow(models_per_segment)) {
      if (str_detect(models_per_segment$model[i], "BMW")) {
        contains_bmw_boolean = T
      }
    }
    
    ggplotly(
      if (contains_bmw_boolean) {
        models_per_segment%>%
          ggplot(aes(model, sales, fill=factor(ifelse(str_detect(model,"BMW"),"BMW","Others"))))+
          geom_col()+
          theme_minimal()+
          theme(axis.text.x = element_text(angle = -25, vjust = 1, hjust=1))+
          scale_fill_manual(name = "model", values=c("#E7222E","#81C4FF"))+
          ggtitle(paste0("Models in ", input$SelectedSegment))+
          theme(legend.position='none')+
          xlab("")
      }
      else {
        models_per_segment%>%
          ggplot(aes(model, sales))+
          geom_col(fill = "#81C4FF" )+
          theme_minimal() +
          theme(axis.text.x = element_text(angle = -25, vjust = 1, hjust=1))+
          ggtitle(paste0("Models in ", input$SelectedSegment))+
          theme(legend.position='none')+
          xlab("")
          
      }
    , tooltip=c("x","y"))
  })
  
#TAB 3: MARKET TRENDS----------------------------------------------------------------------------------------------------------------
  
  output$fueltype_map <- renderPlotly({
    
    cars_by_fuel_type%>%
      select(region, Total, Time, Petroleum_Products, LPG, Diesel, Natural_Gas, Electricity, Alternative_Energy)%>%
      gather(key = "FuelType", value = "amount", -region, -Total, -Time)%>%
      filter(FuelType == input$selectFuelType)%>%
      mutate(relative_frequency = round(amount/Total * 100, 2)) -> filtered_fuel_type
    
    maximum_relative_fueltype <- max(filtered_fuel_type$relative_frequency, na.rm = T )
    
    filtered_fuel_type <- filtered_fuel_type%>%
      filter(Time == input$SelectedYear_fuel_type)
      
    
    europeCoords$value <- filtered_fuel_type$relative_frequency[match(europeCoords$region, filtered_fuel_type$region)]
    
    ggplotly(
      ggplot() + 
        geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value), colour = "black", size = 0.1) +
        coord_map(xlim = c(-13, 35),  ylim = c(32, 71))+
        scale_fill_gradientn(name = "relative percentage of cars", 
                             colours = c("#E7222E","#81C4FF"),
                             na.value = "grey50",
                             limits=c(0, maximum_relative_fueltype))+
        theme_minimal()+
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), axis.title = element_blank(),
              plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))
    )
  })
  
  sliderValues <- reactive({
    data.frame(
      Name = "SelectedYear_fuel_type",
      Value = as.character(input$"SelectedYear_fuel_type"),
      stringsAsFactors = FALSE
    )
  })
  
  output$bmw_group_sales <- renderPlotly({
    predictive_sales_bmwgroup <- sales_annual_bmwgroup%>%
      filter(Year == 2020 | Year == 2021 | Year == 2022 | Year == 2023)
    
    ggplotly(
      sales_annual_bmwgroup %>%
        filter(Year != 2021 & Year != 2022 & Year != 2023)%>%
        ggplot() +
        geom_line(aes(Year, Sales))+
        geom_point(aes(Year, Sales)) +
        geom_point(data = predictive_sales_bmwgroup, aes(Year, Sales), color = "#16588E") +
        geom_line(data = predictive_sales_bmwgroup, aes(Year, Sales), color = "#16588E") +
        theme_minimal()+
        xlab("Year") +
        ylab("Sales")
    )
  })
  

  output$bmw_brand_sales <- renderPlotly({
    predictive_sales_bmw <- sales_bmw%>%
      filter(Year == 2020 | Year == 2021 | Year == 2022 | Year == 2023)%>%
      mutate(Sales = TotalYear)
    
    ggplotly(
      sales_bmw%>%
        mutate(Sales = TotalYear)%>%
        select(Sales, Year)%>%
        filter(Year != 2021 & Year != 2022 & Year != 2023)%>%
        full_join(annual_sales_bmw, by = c("Year", "Sales"))%>%
        ggplot() +
        geom_point(aes(Year, Sales))+
        geom_line(aes(Year, Sales))+
        geom_point(data = predictive_sales_bmw, aes(Year, Sales), color = "#16588E") +
        geom_line(data = predictive_sales_bmw, aes(Year, Sales), color = "#16588E") +
        theme_minimal() +
        xlab("Year") +
        ylab("Sales")
    ,tooltip = c("x", "y"))
  })
  
  output$bmw_model_sales <- renderPlotly({
    ggplotly(
      sales_by_model %>%
        filter(Model == input$selectModel) %>%
        group_by(year, Model) %>%
        summarise(Sales = sum(Sales, na.rm = T)) %>%
        ggplot(aes(year, Sales, color = Model)) +
        geom_line() +
        geom_point() +
        theme_minimal()+
        xlab("Year") +
        ylab("Sales")
    )
  })
  
  
  
 
  output$google_trends <- renderPlotly({
    GT_terms_sep <- ifelse(str_detect(input$GT_Terms, ","), strsplit(input$GT_Terms, ", "), input$GT_Terms)
    trends <- gtrends(keyword = GT_terms_sep, time = input$GT_Time)
    
    ggplotly(
      trends$interest_over_time%>%
        mutate(hits = ifelse(hits == "<1", 0, hits),
               hits = as.numeric(hits),
               geo = as.factor(geo),
               keyword = as.factor(keyword))%>%
        filter(geo == "world")%>%
        ggplot(aes(date, hits, color = keyword))+
        geom_line()
    )
  })
  
  output$distribution_segments <- renderPlotly({
    # de derde aes vb fill orderen dat kan alleen alphabetisch en niet op size dat is duidelijk uit mijn uitgebreid googlen
    
    segment_2018 <- full_segment_sales %>%
      filter(year == 2018)%>%
      group_by(type) %>%
      summarize(Sales = sum(sales, na.rm = T)) %>%
      mutate(Rel_Sales = round((Sales / sum(Sales, na.rm = T)) * 100, 2),
             year = 2018)%>%
      arrange(desc(Sales))
    
    for (i in 1:nrow(segment_2018)) {
      segment_2018$alphabetically[i] <- intToUtf8((123 - i))
    }

    segment_2019 <- full_segment_sales %>%
      filter(year == 2019)%>%
      group_by(type) %>%
      summarize(Sales = sum(sales, na.rm = T)) %>%
      mutate(Rel_Sales = round((Sales / sum(Sales, na.rm = T)) * 100, 2),
             year = 2019)%>%
      arrange(desc(Sales))

    segment_2020 <- full_segment_sales %>%
      filter(year == 2020)%>%
      group_by(type) %>%
      summarize(Sales = sum(sales, na.rm = T)) %>%
      mutate(Rel_Sales = round((Sales / sum(Sales, na.rm = T)) * 100, 2),
             year = 2020)%>%
      arrange(desc(Sales))
    
    segment_2019 <- segment_2018%>%
      select(type, alphabetically)%>%
      right_join(segment_2019, by = "type")
    
    segment_2020 <- segment_2018%>%
      select(type, alphabetically)%>%
      right_join(segment_2020, by = "type")
    
    segment_complete <- rbind(segment_2018, segment_2019)
    segment_complete <- rbind(segment_complete, segment_2020)
    
    ggplotly(segment_complete%>%
               ggplot()+
               geom_col(aes(year, Sales, fill = alphabetically, text = paste("Segment: ", segment_complete$type,
                                                                             "\nRelative Sales: ", Rel_Sales, "%")), color = "black")+
               # scale_fill_manual(name = "Segment",
               #                   values = c("#E7222E", "#E7222E", "#E7222E", "#E7222E", "#E7222E", "#E7222E", "#E7222E", "#E7222E", "#E7222E",
               #                              "#81C4FF", "#81C4FF", "#81C4FF", "#16588E", "#16588E"),
               #                   breaks = c("m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"),
               #                   labels = c("exotic_sport_car", "upperclass_car", "largeMPV",
               #                              "electric_vehicle", "passenger_van", "largeSUV", "large_car", "midsizedMPV",
               #                              "midsizedCrossover", "midsized_car", "mini_cars", "smallCrossover", "compact_car", "subcompact_car"))+
               theme_minimal()+
               theme(legend.position = "none"),
             tooltip = c("x" , "y", "text")
             )
  })
  
#TAB 4:Customer satisfaction---------------------------------------------------------------------------------------------------------------

  output$overallCustomerSat <- renderPlotly({
    ggplotly(
      customerSatisfactionBenchark %>%
        filter(brand %in% input$selectBrand) %>%
        ggplot()+
        geom_line(aes(year, satisfactionScore, color = brand), size = 1.5)+
        theme_minimal()+
        scale_color_manual(values = c("#E7222E","#81C4FF","#16588E"))+
        ylim(50,100)
      )
    })
  
  output$detailedCustomerSat <- renderPlotly({
    ggplotly(
      satisfaction_per_brand%>%
        filter(Brand %in% input$selectBrand)%>%
        filter(type %in% input$selectSatisfactionType)%>%
        ggplot()+
        geom_col(aes(x = "", y = score, fill = Brand), position = "dodge")+
        theme_minimal()+
        scale_fill_manual(values = c("#E7222E","#81C4FF","#16588E"))+
        theme(axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank())+
        facet_wrap(~type),
      tooltip = c("y","fill")
    )
  })
  
  output$customer_loyalty <- renderPlotly({
    ggplotly(
      brand_loyalty %>%
        mutate(Brand = fct_reorder(Brand, Loyalty_perc)) %>%
        ggplot() +
        geom_col(aes(Brand, Loyalty_perc, fill=factor(ifelse(str_detect(Brand,"BMW"),"BMW","Others")))) +
        theme_minimal()+
        scale_fill_manual(name = "model", values=c("#E7222E","#81C4FF")) +
        coord_flip() +
        xlab("") +
        ylab("Percentage of loyal customers"),
      tooltip = c("x","y")
    ) %>% layout(showlegend = F)
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
