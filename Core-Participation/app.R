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
                      h1("Does 70/30 mean 70/30?"),
                      p("The MESM class of 2020 is 70% women and 30% men. Does this lead to 70% participation by women and 30% by men? Our app explores how gender affects participation in Bren core classes in the Fall of 2018. We visualize changes in participation by gender as the quarter progresses, estimate the probabilty of women and men participating under different circumstances, and calculate probabilities of different gender identities being called on based on the professor's gender.")
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
                          checkboxGroupInput("class_time_model",
                                             label = "Section of Class",
                                             choices = list("Beginning" = "1",
                                                            "Middle" = "2",
                                                            "End" = "3"),
                                             selected = "1"),
                          radioButtons("prof_gender_model",
                                       label = "Professor's Gender",
                                       choices = list("Male" = "0", "Female" = "1"),
                                       selected = "m"),
                          radioButtons("q_a_model",
                                       label = "Type of Participation",
                                       choices = list("Question" = "q",
                                                      "Answer" = "a"),
                                       selected = "q")
                          
                        ),
                        #main panel
                        mainPanel(
                          h2("Probability of Woman Participation with the Choosen Parameters"),
                          textOutput("selected_model")
                        )
                      )),
             
             tabPanel("Contested Call",
                      radioButtons("ContCallGender", 
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
      filter(date >= input$date_range[1], date <= input$date_range[2]) %>% 
      filter(student_g_p == input$Gender_time[1]| student_g_p == input$Gender_time[2])
  })
  
  # time series panel
  output$time_plot <- renderPlot(
    {
        ggplot(datareact_time(), aes(x = date))+
        geom_bar(aes(fill = student_g_p)) +
        scale_fill_manual(limits = c("m", "w"), values = c("skyblue1", "palevioletred1"), name = "Student Gender Preference", labels = c("Male", "Female")) +
        facet_wrap(~class) +
        theme_classic() +
        scale_y_continuous(expand = c(0,0), limits = c(0,35), breaks = seq(0,35, by = 5)) +
        scale_x_date(breaks = as.Date(c("2018-10-01", "2018-10-08", "2018-10-15", "2018-10-22", "2018-10-29", "2018-11-05", "2018-11-12", "2018-11-19", "2018-11-26", "2018-12-03"))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        labs(x = "Date", y = "Number of participants") 
    }
  )
  
  #Model data 
  data_model <- participation %>% 
    mutate(student_g_binary = recode(student_g_p, "w" = 1, "m" = 0)) %>% 
    select(date, class, professor, prof_g_p, q_or_a, student_g_p, time, student_g_binary)
  
  #Binary Logistic Model
  gender_logmod1 <- glm(student_g_binary ~ class + time + prof_g_p + q_or_a, data = data_model, family = "binomial")
  
  #reaction for model widgets
  datareact_model <- reactive({
    -.1166 + .3345 * (as.numeric(input$prof_gender_model))
    #data.frame(class = input$class_model, time = input$class_time_model, prof_g_p = input$prof_gender_model, q_or_a = input$q_a_model) 
  })
  
  #model panel output
  output$selected_model <- renderText({print(datareact_model())})
    #renderText({predict(gender_logmod1, newdata = datareact_model, type = "response")})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

