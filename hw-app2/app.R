library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)
library(dplyr)
library(DT)
library(stringr)
library(tools)
data <- read.csv('vgsales.csv')
#checking NA's in data
sum(is.na(data))
#removing data that uses a literal 'N/A' string rather than have null values
df=data[!grepl("N/A",data$Year),]
#filtering for data after 2006 because that was the year the PS3 released and 
#I'm more interested in that time up to the present
df <- data %>%
  filter(Year >= 2006)

df$Year <- as.numeric(as.character(df$Year))

str(df)



pdf(NULL)


header <- dashboardHeader(title = "Video Game Sales: 2006 to Present"
                          
                          
                          
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    
    selectInput(inputId = 'y',
                label = 'Y Axis',
                choices = c("NA_Sales" = "NA_Sales",
                            "EU_Sales" = "EU_Sales",
                            "JP_Sales" = "JP_Sales",
                            "Global_Sales" = "Global_Sales"),
                selected = 'Global_Sales'),
    
    sliderInput("YearRelease",
                "Release Year:",
                min = min(df$Year, na.rm = T),
                max = max(df$Year, na.rm = T),
                value = c(min(df$Year, na.rm = T), max(df$Year, na.rm = T)),
                step = 1)
    
  )
)

body <- dashboardBody(#tabItems(
  
 # tabItem("plot",
          
          
          
          fluidRow(
            box(title = "Plot",
                   width = 12,
                   tabPanel("Sales", plotlyOutput("scatter")))
          )


)


ui <- dashboardPage(header, sidebar, body)



server <- function(input, output, session) {
  
  YRInput <- reactive({
    df1 <- df %>%
      
    filter(Year >= input$YearRelease[1] & Year <= input$YearRelease[2])

  })
  
  output$scatter <- renderPlotly({
    ggplotly(
      ggplot(data = YRInput(), aes(x = Year, y = input$y)) +
        geom_point())
  })

  

  
  # output$scatter <- renderPlotly({
  #   ggplotly(
  #     ggplot(data = df, aes_string(x = df$Year, y = input$y)) +
  #       geom_point(alpha = input$alpha),
  #     tooltip = c("x", "y"))
  # })
  
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)