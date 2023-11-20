library(shiny)
library(tidyverse)


# LOAD IN THE DATASET
# This dataset is from James Hoffmann's Great American Taste Test.
# The YouTube video and original link to the dataset can be found here: https://www.youtube.com/watch?v=bMOOQfeloH0.
# The video was published on November 10, 2023
#data <- read_csv(here::here("GACTT_RESULTS_ANONYMIZED_v2.csv")) # use this when deploying to shinyapp.io
data <- read_csv(here::here("coffee_preferences", "GACTT_RESULTS_ANONYMIZED_v2.csv")) # use when running locally

# Define UI for application
ui <- fluidPage(
  titlePanel("James Hoffmann's Great American Taste Test"),
  
  #FEATURE 1: CREATE SIDE PANEL WITH AN INTRODUCTION TO THE DATASET AND CREATE A MAIN PANEL WITH TABS TO DISPLAY DATA.
  #I added this feature, as it is a visually clean way to separate information about the dataset (source, what it includes,
  #why the taste test was done) from plots visualizing the data. 
  sidebarLayout(
    
    sidebarPanel(
      h3("Introduction", style = "font-family: helvetica"),
      p("This app visualizes data collected in James Hoffmann's Great American Taste Test. In this taste test, 
        approximately 4000 participants completed a questionnaire about their coffee preferences and habits. Participants 
        also completed a blind taste test of four different coffees, extracted and provided by Cometeer.", 
        style = "font-family: helvetica"),
      p("Coffees A-C were washed coffees with different roast levels: A was a light roast, B was a medium roast, and C was 
        a dark roast. Coffee D was a light roast, natural processed coffee.", style = "font-family: helvetica"),
      p("The original dataset can be found in the description box of James Hoffmann's video, ''Surprising and Fascinating 
        Results from the Taste Test''.", style = "font-family: helvetica"),
      #FEATURE 2: ADD IMAGE OF JAMES HOFFMANN
      #I added this image to make the app more visually interesting and show users a picture of the person who ran this 
      #taste test!
      img(src = "james_hoffmann_img.png", height = 100, width = 100),
      br(),
      br(),
      a(href="https://www.youtube.com/watch?v=bMOOQfeloH0", "(Original YouTube video linked here)", 
        style = "font-family: helvetica")
    ),
    
    mainPanel(
      
      #FEATURE 3: TAB PANELS
      #I added tabs to separate plots visualizing the demographics data, coffee habits and preferences data, and taste 
      #test results
      tabsetPanel(
        
        #TAB TO DISPLAY DEMOGRAPHICS DATA
        tabPanel("Demographics",
          #FEATURE 4: SELECT PLOT COLOUR
          #I added a select input widget so the user can select the colour of the demographics plot. This makes the app
          #more visually interesting.
          selectInput("selectColour1", label = h5("Select plot colour"), 
                      choices = c("darkgrey","bisque3","darkseagreen4", "cadetblue", "deepskyblue4"), 
                      selected = "darkseagreen4"),
          h3("Participant Demographics", style = "font-family: helvetica"),
          h4("Age", style = "font-family: helvetica"),
          plotOutput("age"),
          br(),
          h4("Gender"),
          plotOutput("gender")
        ),
        
        #TAB TO DISPLAY SOME OF THE COFFEE DRINKING HABITS AND PREFERENCES DATA
        tabPanel("Coffee Habits and Preferences",
          fluidRow(
            #FEATURE 4: SELECT PLOT COLOUR
            #Same as above, but for coffee drinking habits and preferences tab
            selectInput("selectColour2", label = h5("Select plot colour"), 
                        choices = c("darkgrey","bisque3","darkseagreen4", "cadetblue", "deepskyblue4"), 
                        selected = "darkseagreen4"),
            h3("Cofee-Drinking Habits and Preferences", style = "font-family: helvetica"),
            p("Participants were asked a few questions about their coffee drinking habits and preferences. Plot the 
              results below.")
          ),
          
          fluidRow(
            column(4,
                   br(),
                   #FEATURE 5: SELECT WHICH COFFEE DRINKING HABITS AND PREFERENCES DATA TO DISPLAY
                   #I added this radio button widget so users can choose which coffee drinking habits and preferences 
                   #data they would like to visualize, rather than cluttering this tab with many individual static plots. 
                   radioButtons("prefPlot", "What would you like to plot?",
                           choices = c(
                             "`How many cups of coffee do you typically drink per day?`", 
                             "`Before today's tasting, which of the following best described what kind of coffee you like?`", 
                             "`How strong do you like your coffee?`", 
                             "`What roast level of coffee do you prefer?`",
                             "`How much caffeine do you like in your coffee?`"
                             ),
                           selected = "`How many cups of coffee do you typically drink per day?`"),
              style = "border:2px solid black"
            ),
            column(8,
              plotOutput("preference")
            )),
          
          fluidRow(
            h3("Coffee Expertise", style = "font-family: helvetica"),
            p("Participants were also asked about their perceived level of coffee expertise on a scale of 1-10. 
              The results are shown below."),
            plotOutput("expertise")
          )),
        
        #TAB TO DISPLAY KEY TASTE TEST RESULTS
        tabPanel("Taste Test",
          fluidRow(
            h3("Taste Test Results", style = "font-family: helvetica"),
            #FEATURE 4: SELECT PLOT COLOUR
            #Same as above, but for taste test results tab
            selectInput("selectColour3", label = h5("Select plot colour"), 
                        choices = c("darkgrey","bisque3","darkseagreen4", "cadetblue", "deepskyblue4"), 
                        selected = "darkseagreen4"),
            #FEATURE 6: FILTER TASTE TEST RESULTS BY GENDER
            #I added a select input widget so the user can filter the plotted taste test results by participant gender.
            #I thought this would be interesting, as James Hoffmann's original video discussing the findings of this taste 
            #test highlighted that male and female participants had quite different preferences between the coffee types
            selectInput("selectGender", label = h5("Filter for participant gender"), 
                        choices = c("All","Male", "Female", "Other (please specify)", "Non-binary"), 
                        selected = "All"),
            h4("Overall Preference", style = "font-family: helvetica"),
            p("Out of coffees A-D, participants were asked to identify their favourite one. The plot below shows how many 
              participants chose each coffee as their favourite.", style = "font-family: helvetica"),
            plotOutput("overallPref"),
            br()
          ),
          
          fluidRow(
            column(6,
                   h4("Roast Preference", style = "font-family: helvetica"),
                   p("Coffees A-C were all washed coffees with different roast levels. Coffee A was a light roast, B was 
                     a medium roast, and C was a dark roast. Out of coffees A-C, participants were asked to identify their 
                     favourite to evaluate roast preference. Results are plotted below.", style = "font-family: helvetica"),
                   plotOutput("roastPref")
                   ),
            column(6,
                   h4("Process Preference", style = "font-family: helvetica"),
                   p("Cofees A and D were both light roast coffees, but A was a washed coffee and D was a natural processed 
                     (fermented) coffee. Out of coffees A and D, participants were asked to identify their favourite to 
                     evaluate coffee process preference. Results are plotted below.", style = "font-family: helvetica"),
                   plotOutput("processPref")
                   )
          )
        )
      )
    )
  )
)
    
# Define server logic
server <- function(input, output) {
  
  #PLOT PARTICIPANT AGE DATA
  output$age <- renderPlot({
    data %>% ggplot(aes(`What is your age?`)) +
      geom_bar(fill = input$selectColour1) +
      ylab("Number of participants") +
      xlab("Age") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      scale_x_discrete(guide = guide_axis(angle = 45))
  })
  
  #PLOT PARTICIPANT GENDER DATA
  output$gender <- renderPlot({
    data %>% ggplot(aes(Gender)) +
      geom_bar(fill = input$selectColour1) +
      ylab("Number of participants") +
      xlab("Gender") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      scale_x_discrete(guide = guide_axis(angle = 45))
  })
  
  #PLOT THE COFFEE DRINKING HABITS/PREFERENCE DATA SELECTED BY THE USER
  output$preference <- renderPlot({
    ggplot(data, aes_string(input$prefPlot)) +
      geom_bar(fill = input$selectColour2) +
      ylab("Number of participants") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      scale_x_discrete(guide = guide_axis(angle = 45))
  })
  
  #PLOT PARTICIPANTS' SELF-REPORTED COFFEE EXPERTISE
  output$expertise <- renderPlot({
    ggplot(data = subset(data, !is.na(`Lastly, how would you rate your own coffee expertise?`)), 
           aes(`Lastly, how would you rate your own coffee expertise?`, na.rm = TRUE)) +
      geom_bar(fill = input$selectColour2) +
      ylab("Number of participants") +
      xlab("Level of coffee expertise") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) 
  })
  
  #FILTER THE TASTE TEST RESULT DATA BY GENDER, IF SELECTED BY USER
  data3 <- reactive({
    if (input$selectGender == "All"){
      data
    }
    else {
      data %>%
        filter(Gender == input$selectGender)
    }
  })
  
  #PLOT OVERALL COFFEE PREFERENCE RESULTS FROM TASTE TEST
  output$overallPref <- renderPlot({
    ggplot(data = subset(data3(), !is.na(`Lastly, what was your favorite overall coffee?`)), 
           aes(`Lastly, what was your favorite overall coffee?`)) +
      geom_bar(fill = input$selectColour3) +
      ylab("Number of participants") +
      xlab("Favourite overall coffee") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      scale_x_discrete(guide = guide_axis(angle = 45))
  })
  
  #PLOT COFFEE ROAST PREFERENCE RESULTS FROM TASTE TEST
  output$roastPref <- renderPlot({
    ggplot(data = subset(data3(), !is.na(`Between Coffee A, Coffee B, and Coffee C which did you prefer?`)), 
           aes(`Between Coffee A, Coffee B, and Coffee C which did you prefer?`)) +
      geom_bar(fill = input$selectColour3) +
      ylab("Number of participants") +
      xlab("Roast preference") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      scale_x_discrete(guide = guide_axis(angle = 45))
  })
  
  #PLOT COFFEE PROCESS PREFERENCE RESULTS FROM TASTE TEST
  output$processPref <- renderPlot({
    ggplot(data = subset(data3(), !is.na(`Between Coffee A and Coffee D, which did you prefer?`)), 
           aes(`Between Coffee A and Coffee D, which did you prefer?`)) +
      geom_bar(fill = input$selectColour3) +
      ylab("Number of participants") +
      xlab("Process preference") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      scale_x_discrete(guide = guide_axis(angle = 45))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
