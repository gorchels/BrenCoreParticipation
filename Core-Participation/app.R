#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)

participation <- read_csv("data/women_participation_git_fix.csv") 


# Define UI for application
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
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
                                         start = "2018-10-01",
                                         end = "2018-12-07",
                                         min = "2018-10-01",
                                         max = "2018-12-07"
                                         ),
                          checkboxGroupInput("Gender_time", 
                                       label = "Select Student Gender Preference",
                                       choices = list("Male" = "m", "Female" = "w"),
                                       selected = "m"
                                       )
                        ),
                        #main panel
                        mainPanel( 
                          h2("Student Participation in Core Classes"),
                          plotOutput("time_plot")
                        )
                      )
      
                      ),
             
             tabPanel("Model",
                      #sidebar with input widgets
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("class_model",
                                      label = "Bren Core Class",
                                      choices = list("Earth System Science" = "203", 
                                                     "Data Analysis" = "206", 
                                                     "Buisness" = "210"),
                                      selected = "203"), 
                          radioButtons("prof_gender_model",
                                       label = "Professor's Gender",
                                       choices = list("Male" = "m", "Female" = "w"),
                                       selected = "m")
                          
                        ),
                        #main panel
                        mainPanel(
                          h2("Probability of Woman Participation with the given parameters"),
                          textOutput("selected_model")
                        )
                      )),
             
             tabPanel("Contested Call",
                      radioButtons("Gender", 
                                   label = "Professor's Gender",
                                   choices = list("Male" = 1, "Female" = 2),
                                   selected = 0)
             
             )
)
)
   

server <- function(input, output) {
  
  #reaction for time graph widgets
  datareact_time <- reactive({
    participation %>% 
      mutate(date = mdy(date)) %>% 
      select(date, student_g_p, class) %>% 
      filter(student_g_p == input$Gender_time) %>% 
      filter(date >= input$date_range[1], date <= input$date_range)
  })
  
  # time series panel
  output$time_plot <- renderPlot(
    {
        ggplot(datareact_time(), aes(x = date))+
        geom_bar(aes(fill = student_g_p)) +
        scale_fill_manual(limits = c("m", "w"), values = c("skyblue1", "palevioletred1"), name = "Student Gender Preference", labels = c("Male", "Female")) +
        facet_wrap(~class) +
        theme_classic() +
        #scale_y_continuous(expand = c(0,0), limits = c(0,18), breaks = seq(0,20, by = 5)) +
        scale_x_date(breaks = as.Date(c("2018-10-01", "2018-10-08", "2018-10-15", "2018-10-22", "2018-10-29", "2018-11-05", "2018-11-12", "2018-11-19", "2018-11-26", "2018-12-03"))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        labs(x = "Date", y = "Number of participants") 
    }
  )
  
  #model panel output
  output$selected_model <- renderText({})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

