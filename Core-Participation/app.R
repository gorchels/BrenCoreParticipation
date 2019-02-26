#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)

participation <- read_csv("data/women_participation_git_fix.csv") 


# Define UI for application
ui <- fluidPage(
  
  #Application title
  titlePanel("Exploring Bren Core Participation by Gender"),
  
  navbarPage(" ",
             
             tabPanel("Summary",
                      h1("Title of the app"),
                      p("Description of the app")
                      ),
             
             tabPanel("Daily Particiaption",
                      #sidebar with input widgets
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("date_range",
                                         label = "Select Date Range",
                                         start = "2018-10-02",
                                         end = "2018-12-06",
                                         min = "2018-10-02",
                                         max = "2018-12-06"
                                         ),
                          checkboxGroupInput("Gender", 
                                       label = "Select Student Gender Preference",
                                       choices = list("Male" = "m", "Female" = "w"),
                                       selected = "m"
                                       )
                        ),
                        #main panel
                        mainPanel(
                          plotOutput("time_plot")
                        )
                      )
      
                      ),
             
             tabPanel("Model"),
             
             tabPanel("Contested Call")
)
)
   

server <- function(input, output) {
  
  datareact_time <- reactive({
    participation %>% 
      mutate(date = mdy(date)) %>% 
      filter(student_g_p == input$Gender)
  })
  
  # time series panel
  output$time_plot <- renderPlot(
    {
        ggplot(datareact_time(), aes(x = date))+
        geom_bar(aes(fill = student_g_p)) +
        facet_wrap(~class)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

