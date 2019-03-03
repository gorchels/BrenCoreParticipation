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
                          radioButtons("Gender", 
                                       label = "Select Student Gender Preference",
                                       choices = list("Male" = 1, "Female" = 2, "Both" = 3),
                                       selected = 0
                                       )
                        ),
                        #main panel
                        mainPanel(
                          plotOutput("time_plot")
                        )
                      )
      
                      ),
             
             tabPanel("Model"),
             
             tabPanel("Contested Call",
                      radioButtons("Gender", 
                                   label = "Professor's Gender",
                                   choices = list("Male" = 1, "Female" = 2),
                                   selected = 0) 
             
             )
)
)
   

server <- function(input, output) {
  
  # time series panel
  output$time_plot <- renderPlot(
    {
      
      
      ggplot(participation, aes(x = date))+
        geom_bar(aes(fill = student_g_p)) +
        facet_wrap(~class)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

