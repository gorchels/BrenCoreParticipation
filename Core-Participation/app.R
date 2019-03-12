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
library(png)

participation <- read_csv("data/women_participation_git_fix.csv") 

con_prop = data.frame(
  prof_g = c("m", "w", "m", "w"), 
  stud_g = c("w", "w", "m", "m"), 
  pie = c(53, 41, 47, 59))

bren = readPNG("data/brenlogo.png")

# Define UI for application
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  #Application title
  titlePanel("Exploring Bren Core Participation by Gender"),
  
  navbarPage(" ",
             
             tabPanel("Summary",
                      h2("What does this App explore?"),
                      p("The MESM class of 2020 is 70% women and 30% men. Does this lead to 70% participation by women and 30% by men? Our app explores how gender affects participation in Bren core classes in the Fall of 2018. We visualize changes in participation by gender as the quarter progresses, estimate the probabilty of women and men participating under different circumstances, and calculate probabilities of different gender identities being called on based on the professor's gender."),
                      h2("The Data"),
                      p("The data used for this app is observational count data on the participation of the 2020 cohort of Bren students during the fall core classes Earth System Science, Data Analysis, and Business. Each class Madeline Gorchels (a class of 2020 Bren student) tallied the gender of participants, the section of class the participation occurred, and if a contested call occurred. The dataset included about 600 observation entries."),
                      mainPanel(
                        img(src= bren, align = "left")
                      )
             ),
             
             tabPanel("Daily Participation",
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
                                       choices = list("Man" = "m", "Woman" = "w"),
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
                                                     "Business" = "210"),
                                      selected = "203"), 
                          selectInput("class_time_model",
                                             label = "Section of Class Time",
                                             choices = list("Beginning" = "1",
                                                            "Middle" = "2",
                                                            "End" = "3"),
                                             selected = "1"),
                          radioButtons("prof_gender_model",
                                       label = "Professor's Gender",
                                       choices = list("Man" = "0", "Woman" = "1"),
                                       selected = "0"),
                          radioButtons("q_a_model",
                                       label = "Type of Participation",
                                       choices = list("Question" = "1",
                                                      "Answer" = "0"),
                                       selected = "1")
                          
                        ),
                        #main panel
                        mainPanel(
                          h2("Probability of Woman Participation with the Choosen Parameters"),
                          h1(textOutput("selected_model"), align = "center")
                        )
                      )),
             
             tabPanel("Contested Call",
                      sidebarLayout(
                        sidebarPanel(p("A contested call occurs when one man and one woman raises their hand. The proportion shows the number of times a student was selected by gender."),
                      radioButtons("ContCallGender", 
                                   label = "Professor's Gender",
                                   choices = list("Man" = "m", "Woman" = "w"),
                                   selected = "w")
                      
                    )
                    ,
                      mainPanel( 
                        h2("Proportion Women Called On"),
                        plotOutput("con_plot"))
             
             ))
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
        scale_fill_manual(limits = c("m", "w"), values = c("paleturquoise3", "darkgreen"), name = "Student Gender Preference", labels = c("Male", "Female")) +
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
    select(date, class, professor, prof_g_p, q_or_a, student_g_p, time, student_g_binary) %>% 
    mutate(class = as.factor(class)) %>% 
    mutate(time = as.factor(time))
  
  #Binary Logistic Model
  gender_logmod1 <- glm(student_g_binary ~ class + time + prof_g_p + q_or_a, data = data_model, family = "binomial")
  
  #reaction for model widgets
  datareact_model <- reactive({
    -0.06133 + (0.3345 * (as.numeric(input$prof_gender_model))) - (0.24419 * (as.numeric(input$q_a_model))) + (0.41634 * (as.numeric(if(input$class_model == "206") {1} else {0}))) + (0.28538 * (as.numeric(if(input$class_model == "210") {1} else {0}))) - (0.34415 * (as.numeric(if(input$class_time_model == "2") {1} else {0}))) + (0.17847 * (as.numeric(if(input$class_time_model == "3") {1} else {0}))) 
    #data.frame(class = input$class_model, time = input$class_time_model, prof_g_p = input$prof_gender_model, q_or_a = input$q_a_model) 
  })
  
  #model panel output, in Probability
  output$selected_model <- renderText({print(round(((exp(datareact_model()) / (1 + exp(datareact_model())))*100), digits = 2))})
    #renderText({predict(gender_logmod1, newdata = datareact_model, type = "response")})
  
  #contested call reactive widget
  con_react = reactive({con_prop %>% 
      filter(prof_g == input$ContCallGender)})
  
  #constested call output
  output$con_plot = renderPlot(
    {ggplot(con_react(), aes(x="", y=pie, fill=stud_g))+
        geom_bar(width = 1, stat = "identity")+
        coord_polar("y", start=0)+
        theme_classic()+
        theme(axis.line = element_blank(),
              axis.text = element_blank())+
        labs(x =NULL, y = NULL)+
        scale_fill_manual(limits = c("m", "w"), values = c("paleturquoise3", "darkgreen"), name = "Student Gender Preference", labels = c("Man", "Woman"))})
      
}

# Run the application 
shinyApp(ui = ui, server = server)

