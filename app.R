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
         HTML("This app allows the uploading of energy data in a particular format and performs an analysis of that data. The expected format is an Excel spreadsheet with columns <b>Date</b>, <b>Start Time</b>, <b>Duration</b>, <b>Value</b>, and <b>Year</b>. Once the data has been uploaded, the results will be computed and displayed automatically. The date range can be selected to subset the time frame considered."),
         
         hr(),
         
         h4("Configuration"),
         conditionalPanel(condition = "input.tabs1 == 'Electricity'",
                          fileInput("elec", "Electricity Data", accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
                          selectInput("aggregation", "Aggregation Level", choices = c("Hourly" = "Hour", "Daily" = "Day", "Weekly" = "Week"), selected = "Daily"),
                          dateRangeInput("dates",
                                         "Date Range",
                                         start = "2015-11-09",
                                         end = "2017-10-30")
         )
      ),
      
      mainPanel(
          tabsetPanel(id = "tabs1",
              tabPanel("Electricity",
                       h4("Electricity Usage by Month"),
                       withSpinner(plotlyOutput("monthPlot", height = "600px")),
                       
                       hr(),
                       
                       h4("Electricity Usage by Day of Week"),
                       withSpinner(plotlyOutput("weekPlot", height = "600px"))
              ),
              tabPanel("Gas")
          )
      )
   )
)

server <- function(input, output, session) {
  
  observe({
    if (!is.null(cleaned_elec())) {
      updateDateRangeInput(session, "dates", 
                           min = min(as.Date(cleaned_elec()$SDate)), 
                           max = max(as.Date(cleaned_elec()$SDate)), 
                           start = min(as.Date(cleaned_elec()$SDate)), 
                           end = max(as.Date(cleaned_elec()$SDate)))
    }
  }, suspended = TRUE)
   
   elec_data <- reactive({
     if (is.null(input$elec)) return(NULL)
     
     return(read_xlsx(input$elec$datapath))
   })
   
   cleaned_elec <- reactive({
     if (is.null(elec_data())) return(NULL)
       
        mygroup <- c("Year", "Month", "WeekDay", input$aggregation)
       
        elec_data() %>% 
            select(-Year) %>%
            mutate(SDate = ymd_h(paste(Date, `Start Time` / 100, sep = "-")),
                   Hour = hour(SDate),
                   Day = yday(SDate),
                   Week = week(SDate),
                   WeekDay = wday(SDate, label = TRUE),
                   Month = month(SDate, label = TRUE),
                   Year = year(SDate)) %>%
            group_by_at(vars(one_of(mygroup))) %>%
            summarise(Value = sum(Value),
                      SDate = SDate[1])
   })
   
   filtered_clean_elec <- reactive({
     if (is.null(cleaned_elec())) return(NULL)
       
     cleaned_elec() %>%
        filter(SDate >= input$dates[1], SDate <= input$dates[2])
   })
   
   agg_elec <- reactive({
       if (is.null(filtered_clean_elec())) return(NULL)
       
       mydat <- filtered_clean_elec()
       mydat$myvar <- unlist(mydat[,input$aggregation])
       
       return(mydat)
   })
   
   output$monthPlot <- renderPlotly({
     if(is.null(cleaned_elec())) return(NULL)
           
     g <- ggplot(data=agg_elec(),aes(x=myvar, y=Value, group=myvar, text=SDate)) +
         xlab(input$aggregation) +
       geom_point(size = 0.3) + facet_wrap(~Month, scales = "free_x") +
       theme_bw()
     
     ggplotly(g, tooltip = "text")
   })
   
   output$weekPlot <- renderPlotly({
     if(is.null(cleaned_elec())) return(NULL)
     
       g <- ggplot(data=agg_elec(),aes(x=myvar, y=Value, group=myvar, text=SDate)) +
                    xlab(input$aggregation) +
                    theme_bw() + geom_point(size = 0.3) + facet_wrap(~WeekDay, scales = "free_x")
       
       ggplotly(g, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)
