library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(plotly)
library(lubridate)
library(shinycssloaders)
library(shinythemes)

addResourcePath("images", "images")

ui <- fluidPage(theme = shinytheme("cerulean"),
   
   titlePanel("Energy Usage Viewer"),
   
   sidebarLayout(
      sidebarPanel(
         a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
         
         h4("About"),
         HTML("This app allows the uploading of energy data in a particular format and performs an analysis of that data. The expected format for the electricity data is an Excel spreadsheet with columns <b>Date</b>, <b>Start Time</b>, <b>Duration</b>, <b>Value</b>, and <b>Year</b>. Once the data has been uploaded, the results will be computed and displayed automatically. The date range can be selected to subset the time frame considered."),
         
         hr(),
         
         h4("Configuration"),
         conditionalPanel(condition = "input.tabs1 == 'Electricity'",
                          fileInput("elec", "Electricity Data", accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
                          dateRangeInput("dates",
                                         "Electricity Date Range",
                                         start = "2015-11-09",
                                         end = "2017-10-30")
         ),
         conditionalPanel(condition = "input.tabs1 == 'Gas'",
                          fileInput("gas", "Gas Data", accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
                          dateRangeInput("gas_dates",
                                         "Gas Date Range",
                                         start = "2012-12-10",
                                         end = "2017-10-30")
         )
      ),
      
      mainPanel(
          tabsetPanel(id = "tabs1",
              tabPanel("Electricity",
                       h4("Electricity Usage Over Time"),
                       selectInput("aggregation", "Aggregation Level", choices = c("Daily", "Weekly", "Monthly"), selected = "Monthly"),
                       withSpinner(plotlyOutput("time_series")),
                       hr(),
                       h4("Hourly Electricity Usage"),
                       selectInput("facet", "Grouping", choices = c("None", "WeekDay", "Month")),
                       withSpinner(plotlyOutput("hourly_plot", height = "600px"))
              ),
              tabPanel("Gas",
                       h4("Gas Usage Over Time"),
                       withSpinner(plotlyOutput("gas_time_series"))
              )
          )
      )
   )
)

server <- function(input, output, session) {
  
  observe({
    if (!is.null(elec_data())) {
      updateDateRangeInput(session, "dates", 
                           min = min(as.Date(elec_data()$Date)), 
                           max = max(as.Date(elec_data()$Date)), 
                           start = min(as.Date(elec_data()$Date)), 
                           end = max(as.Date(elec_data()$Date)))
    }
      
      if (!is.null(gas_series())) {
          updateDateRangeInput(session, "gas_dates", 
                               min = min(as.Date(gas_series()$`Start Date`)), 
                               max = max(as.Date(gas_series()$`End Date`)), 
                               start = min(as.Date(gas_series()$`Start Date`)), 
                               end = max(as.Date(gas_series()$`End Date`)))
      }
  }, suspended = TRUE)
   
   elec_data <- reactive({
     if (is.null(input$elec)) return(NULL)
     
     return(read_xlsx(input$elec$datapath))
   })
   
   elec_series <- reactive({
     if (is.null(elec_data())) return(NULL)
       
       myagg <- switch(input$aggregation, "Daily" = c("Day", "Month", "Year"), 
                       "Weekly" = c("Week", "Year"),
                       "Monthly" = c("Month", "Year"))
       
        elec_data() %>% 
            mutate(Date = as.Date(Date)) %>%
            filter(Date >= input$dates[1], Date <= input$dates[2]) %>%
            mutate(SDate = ymd(Date),
                   Day = yday(SDate),
                   Week = week(SDate),
                   Month = month(SDate, label = TRUE),
                   Year = year(SDate)) %>%
            group_by_at(vars(one_of(myagg))) %>%
            summarise(Usage = sum(Value),
                      Date = SDate[1]) %>%
            arrange(Date)
   })
   
   elec_hourly <- reactive({
       if (is.null(elec_data())) return(NULL)
           
       elec_data() %>%
           mutate(Date = as.Date(Date)) %>%
           filter(Date >= input$dates[1], Date <= input$dates[2]) %>%
           mutate(SDate = ymd(Date),
                  WeekDay = wday(SDate, label = TRUE),
                  Month = month(SDate, label = TRUE),
                  Hour = factor(paste0(`Start Time` / 100, ":00"), levels = paste0(0:23, ":00")),
                  Usage = Value)
   })
   
   output$time_series <- renderPlotly({
       if (is.null(elec_series())) return(NULL)
       
       g <- ggplot(data = elec_series(), aes(x = Date, y = Usage)) +
           geom_line() +
           theme_bw()
       
       if (input$aggregation != "Daily") g <- g + geom_point()
       
       ggplotly(g)
   })
   
   elec_grouped <- reactive({
       if (is.null(elec_data())) return(NULL)
       
       myfunc <- get(input$facet)
   })
   
   output$hourly_plot <- renderPlotly({
     if (is.null(elec_hourly())) return(NULL)
           
     g <- ggplot(data = elec_hourly(), aes(x = Hour, y = Usage, text = SDate)) +
         geom_boxplot() +
         geom_point(size = 0.3, alpha = 0) +
         theme_bw() +
         theme(axis.text.x = element_text(angle = 45, size = 6))
     
     if (input$facet != "None") g <- g + facet_wrap(as.formula(paste("~", input$facet)), scales = "free_x")
     
     ggplotly(g)
   })
   
   gas_data <- reactive({
       if (is.null(input$gas)) return(NULL)
       
       return(read_xlsx("data/Portfolio_Warner Memorial Presbyterian Church.xlsx", sheet = 6, skip = 5))
   })
   
   gas_series <- reactive({
       if (is.null(gas_data())) return(NULL)
       
       all_agg <- gas_data() %>%
           select(`Meter Type`, `Start Date`, `End Date`, `Usage/Quantity`, `Usage Units`, `Cost ($)`) %>%
           mutate(SDate=as.Date(`Start Date`), EDate=as.Date(`End Date`), Diff_Days=EDate-SDate) %>%
           filter(SDate >= input$gas_dates[1], EDate <= input$gas_dates[2])
       
       all_agg %>% # Take the agg data
           mutate(Cost = replace(`Cost ($)`, `Cost ($)` == "Not Available", NA)) %>% # Set the not available costs to NA
           rowwise() %>% # Process each row (no grouping)
           do(date_seq = seq(.$SDate, .$EDate, by = "1 day"), # Create column where each element is every day of the billing period
              `Meter Type` = .$`Meter Type`[1], # Store meter type
              Usage = .$`Usage/Quantity`[1], # Store Usage
              Cost = .$Cost[1]) %>% # Store cost
           mutate(`Meter Type` = unlist(`Meter Type`), # Convert back from a list
                  Usage = Usage / length(date_seq), # Divide usage by the number of days in the billing period
                  Cost = as.numeric(Cost) / length(date_seq)) %>% # Divide cost by the number of days in the billing period
           unnest() %>% # Unnest the data frame (this expands it to one row per every day in date_seq)
           group_by(`Meter Type`, Year = year(date_seq), Month = month(date_seq)) %>% # For each year and month
           summarise(Usage = sum(Usage), Cost = sum(Cost)) %>% # Sum up the per day usage to count the amount for each month
           mutate(Date = ymd(paste(Year, Month, "01", sep = "-"))) %>% # Create a date column (assuming first of the month)
           filter(`Meter Type` == "Natural Gas")
   })
   
   output$gas_time_series <- renderPlotly({
       if (is.null(gas_series())) return(NULL)
       
       g <- ggplot(data = gas_series(), aes(x = Date, y = Usage)) +
           geom_line() +
           theme_bw() +
           geom_point()
       
       ggplotly(g)
   })
}

shinyApp(ui = ui, server = server)
