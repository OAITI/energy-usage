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
   
   # Application title
   titlePanel("Energy Usage Viewer"),
   
   sidebarLayout(
      sidebarPanel(
         a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
         fileInput("elec", "Electricity Data"),
         sliderInput("years",
                     "Years:",
                     min = 2015,
                     max = 2017,
                     value = c(2015, 2017),
                     step = 1)
      ),
      
      mainPanel(
        h4("Hourly Electricity Usage by Month"),
        withSpinner(plotlyOutput("monthPlot")),
        hr(),
        h4("Hourly Electricity Usage by Day of Week"),
        withSpinner(plotOutput("weekPlot"))
      )
   )
)

server <- function(input, output, session) {
  
  observe({
    if (!is.null(cleaned_elec())) {
      updateSliderInput(session, "years", min = min(year(cleaned_elec()$SDate)), max = max(year(cleaned_elec()$SDate)), value = c(min(year(cleaned_elec()$SDate)), max(year(cleaned_elec()$SDate))))
    }
  })
   
   elec_data <- reactive({
     if(is.null(input$elec))  return(NULL)
     
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
      filter(Year %in% input$years)
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

# Run the application 
shinyApp(ui = ui, server = server)
