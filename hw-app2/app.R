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


ui <- dashboardPage(
  
  dashboardHeader(title = "Video Game Sales Data 2006-"),
  
              dashboardSidebar(
                      
                sidebarMenu(id = 'tabs',
                    menuItem("plots", tabName = "Plots",
                             icon = icon("bar-chart")),
                    menuItem("table", tabName = "Table",
                             icon = icon("table"))
                      )
                
                    ),
  
              dashboardBody(
                tabItems(
                  tabItem(tabName = 'Plots',
                          fluidRow(
                            box(plotlyOutput("scatter")),
                            box(plotlyOutput("lol")),
                          
                            box(title = "Select",
                                selectInput(inputId = 'y',
                                            label = 'Sales (in millions)',
                                            choices = c("NA_Sales" = "NA_Sales",
                                                        "EU_Sales" = "EU_Sales",
                                                        "JP_Sales" = "JP_Sales",
                                                        "Global_Sales" = "Global_Sales"),
                                            selected = 'Global Sales')),
                            box(title = "Slider to subset year",
                                sliderInput("YearRelease",
                                            "Release Year:",
                                            min = min(df$Year, na.rm = T),
                                            max = max(df$Year, na.rm = T),
                                            value = c(min(df$Year, na.rm = T), max(df$Year, na.rm = T)),
                                            step = 1)),
                            box(title = "select Publisher",
                                selectInput(inputId = "Pubs",
                                            label = 'Publisher:',
                                            choices = sort(unique
                                                           (df$Publisher)),
                                            multiple = TRUE,
                                            selectize = TRUE,
                                            selected = c("Nintendo")
                                              
                                            )),
                               
                          ))

                ),
                tabItem(tabName = "Table",
                        fluidPage(
                          box(title = "Games in year subset",
                              DT::dataTableOutput("table"), width = 12))
              ))
  
)#this one is for dashboard page



server <- function(input, output) {
  
  YRInput <- reactive({
    df <- df %>%
    filter(Year >= input$YearRelease[1] & Year <= input$YearRelease[2])
  return(df)
})
  
  output$scatter <- renderPlotly({
    dat <- subset(YRInput())

    ggplotly(
    ggplot(dat, aes_string(x = dat$Year, y = input$y)) +
      geom_point(color = 'blue'))

    })
  
  output$table <- DT::renderDataTable({
    subset(YRInput(), select = c(Name, Platform, Year, Genre,
                                 Publisher, Global_Sales))
  })
  
  
  PbInput <- reactive({
    df <- df %>%
      filter(Publisher == input$Pubs)
    return(df)
  })
  
  output$lol <- renderPlotly({
    dat <- subset(PbInput())
    
    ggplot(dat, aes(x=Genre, y=Global_Sales)) +
      geom_segment( aes(x=Genre, xend=Genre, y=0, yend=Global_Sales), color="skyblue") +
      geom_point( color="orange", size=4, alpha=0.6) +
      theme_light() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())
    
  })
  
}



shinyApp(ui, server)


