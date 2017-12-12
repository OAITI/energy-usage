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
         fileInput("elec", "Electricity Data"),
         dateRangeInput("dates",
                     "Date Range",
                     start = "2015-11-09",
                     end = "2017-10-30")
      ),
      
      mainPanel(
          tabsetPanel(id = "tabs1",
              tabPanel("Electricity",
                       h4("Hourly Electricity Usage by Month"),
                       withSpinner(plotlyOutput("monthPlot")),
                       
                       hr(),
                       
                       h4("Hourly Electricity Usage by Day of Week"),
                       withSpinner(plotOutput("weekPlot"))
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
                           min = min(cleaned_elec()$SDate), 
                           max = max(cleaned_elec()$SDate), 
                           start = min(cleaned_elec()$SDate), 
                           end = max(cleaned_elec()$SDate))
    }
  })
   
   elec_data <- reactive({
     if (is.null(input$elec)) return(NULL)
     
     return(read_xlsx(input$elec$datapath))
   })
   
   cleaned_elec <- reactive({
     if(is.null(elec_data())) return(NULL)
      elec_data() %>% 
        rename(hourly_value = Value) %>%
        mutate(SDate=as.Date(Date),Year=as.factor(Year)) %>%
        mutate(Month=month(SDate, label = T)) %>% 
        mutate(Day=wday(SDate, label=TRUE))
     
   })
   
   filtered_clean_elec <- reactive({
     if(is.null(cleaned_elec())) return(NULL)
     
     cleaned_elec() %>%
      filter(SDate >= input$dates[1], SDate <= input$dates[2])
   })
   
   output$monthPlot <- renderPlotly({
     if(is.null(cleaned_elec())) return(NULL)
     
     g <- ggplot(data=filtered_clean_elec(),aes(x=`Start Time`, y=hourly_value, group=`Start Time`, text=SDate)) +
       geom_point( size=0.3) + facet_wrap(~Month, scales = "free_x") +
       theme_bw()
     
     ggplotly(g, tooltip = "text")
   })
   
   output$weekPlot <- renderPlot({
     if(is.null(cleaned_elec())) return(NULL)
     
      ggplot(data=filtered_clean_elec(),aes(x=`Start Time`, y=hourly_value, group=`Start Time`, text=SDate)) +
        geom_boxplot() + facet_wrap(~Day, scales = "free_x") +
        theme_bw()
  })
}

shinyApp(ui = ui, server = server)
