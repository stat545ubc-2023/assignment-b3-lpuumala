# coffee_preferences_v2
# Due 2023-12-07

library(shiny)
library(tidyverse)
library(shinythemes)

# LOAD IN THE DATASET
# This dataset is from James Hoffmann's Great American Taste Test.
# The YouTube video and original link to the dataset can be found here: https://www.youtube.com/watch?v=bMOOQfeloH0.
# The video was published on November 10, 2023
#data <- read_csv(here::here("GACTT_RESULTS_ANONYMIZED_v2.csv")) # use this when deploying to shinyapp.io
data <- read_csv(here::here("coffee_preferences_v2", "GACTT_RESULTS_ANONYMIZED_v2.csv")) # use when running locally

# Convert some variables to factors to make them easier to work with
data$`What is your age?` <- factor(data$`What is your age?`, levels = c("<18 years old", "18-24 years old", "25-34 years old", "35-44 years old", "45-54 years old", "55-64 years old", ">65 years old"))
data$`What is the most you've ever paid for a cup of coffee?` <- factor(data$`What is the most you've ever paid for a cup of coffee?`, levels = c("Less than $2", "$2-$4", "$4-$6", "$6-$8", "$8-$10", "$10-$15", "$15-$20", "More than $20"))
data$`What is the most you'd ever be willing to pay for a cup of coffee?` <- factor(data$`What is the most you'd ever be willing to pay for a cup of coffee?`, levels = c("Less than $2", "$2-$4", "$4-$6", "$6-$8", "$8-$10", "$10-$15", "$15-$20", "More than $20"))
data$`Approximately how much have you spent on coffee equipment in the past 5 years?` <- factor(data$`Approximately how much have you spent on coffee equipment in the past 5 years?`, levels = c("Less than $20", "$20-$50", "$50-$100", "$100-$300", "$300-$500", "$500-$1000", "More than $1000"))


# Define UI for application
ui <- fluidPage(
  theme = shinythemes::shinytheme("paper"),
  titlePanel("James Hoffmann's Great American Taste Test"),
  
  # FEATURE 1: CREATE SIDE PANEL WITH AN INTRODUCTION TO THE DATASET AND CREATE A MAIN PANEL WITH TABS TO DISPLAY DATA.
  # I added this feature, as it is a visually clean way to separate information about the dataset (source, what it includes,
  # why the taste test was done) from plots visualizing the data. 
  sidebarLayout(
    
    sidebarPanel(
      h3("Introduction"),
      p("This app visualizes data collected in James Hoffmann's Great American Taste Test. In this taste test, 
        approximately 4000 participants completed a questionnaire about their coffee preferences and habits. Participants 
        also completed a blind taste test of four different coffees, extracted and provided by Cometeer."),
      p("Coffees A-C were washed coffees with different roast levels:"),
      tags$ul(
      tags$li("A was a light roast"), 
      tags$li("B was a medium roast"), 
      tags$li("C was a dark roast")
      ),
      p("Coffee D was a light roast, natural processed coffee."),
      p("The original dataset can be found in the description box of James Hoffmann's video, ''Surprising and Fascinating 
        Results from the Taste Test''."),
      # FEATURE 2: ADD IMAGE OF JAMES HOFFMANN
      # I added this image to make the app more visually interesting and show users a picture of the person who ran this 
      # taste test!
      img(src = "james_hoffmann_img.png", height = 100, width = 100),
      br(),
      br(),
      # FEATURE 3 (NEW): ACTION BUTTON LINKING TO JAMES HOFFMAN'S VIDEO ABOUT THE TASTE TEST
      # I added this action button as a more visually appealing way to provide users with a direct link to the original dataset.
      # The link was previously provided as just a hyperlink in the sidebar panel text. 
      actionButton(inputId="link1", label="Original YouTube video", 
                   icon = icon("youtube"), 
                   onclick ="window.open('https://www.youtube.com/watch?v=bMOOQfeloH0')")
    ),
    
    mainPanel(
      
      # FEATURE 4: TAB PANELS
      # I added tabs to separate plots visualizing the demographics data, coffee habits and preferences data, and taste 
      # test results
      tabsetPanel(
        
        # TAB TO DISPLAY DEMOGRAPHICS DATA
        tabPanel("Demographics",
          h3("Participant Demographics"),
          fluidRow(
            column(4,
                   br(),
                   # FEATURE 5 (NEW): SELECT WHICH DEMOGRAPHICS DATA TO DISPLAY
                   # I added this radio button so users could visualize the demographic data they are interested in.
                   # This is a more visually clean option than displaying all of the demographics data as separate static plots.
                   radioButtons("demoPlot", "What would you like to plot?",
                                choices = c(
                                  "Gender" = "Gender", 
                                  "Age" = "`What is your age?`", 
                                  "Education level" = "`Education Level`", 
                                  "Ethnicity/Race" = "`Ethnicity/Race`",
                                  "Employment status" = "`Employment Status`",
                                  "Number of children" = "`Number of Children`",
                                  "Political affiliation" = "`Political Affiliation`"),
                                selected = "Gender"),
                   style = "border:2px solid black"
            ),
            column(8,
                   plotOutput("demographics"),
                   br()
            )),
          
          fluidRow(
            column(12,
                   h3("View the full dataset"))
          ),
          
          fluidRow(
            # FEATURE 6 (NEW): INCLUDE DATA TABLE OF FULL DATASET WITH SOME FILTERING OPTIONS
            # In case users are interested in looking at the raw data and how it was structured, I added this data table.
            column(6,
                   selectInput("gen",
                               "Filter data by gender:",
                               c("All", unique(data$Gender)))),
            column(6,
                   selectInput("age",
                               "Filter data by age:",
                               c("All", unique(as.character(data$`What is your age?`)))))
          ),
          
          DT::dataTableOutput("all_data"),
          
          # FEATURE 7 (NEW): DRAGGABLE ABSOLUTE PANEL FOR SELECTING PLOT COLOUR
          # This allows the user to move the colour selector to a more convenient place if they wish.
          absolutePanel(
            top = 400, left = 0, width = 195,
            draggable = TRUE,
            wellPanel(
              # FEATURE 8: SELECT PLOT COLOUR
              # I added a select input widget so the user can select the colour of the demographics plot. This makes the app
              # more visually interesting.
              selectInput("selectColour1", label = "Select plot colour", 
                          choices = c("darkgrey","bisque3","darkseagreen4", "cadetblue", "deepskyblue4"), 
                          selected = "darkseagreen4")),
            style = "opacity: 1.0"
          ),
        ),
        
        # TAB TO DISPLAY SOME OF THE COFFEE DRINKING HABITS AND PREFERENCES DATA
        tabPanel("Coffee Habits and Preferences",
          fluidRow(
            h3("Cofee-Drinking Habits and Preferences"),
            p("Participants were asked a few questions about their coffee drinking habits and preferences. The results
              are displayed on this tab."),
            # FEATURE 8: SELECT PLOT COLOUR
            # Same as above, but for coffee drinking habits and preferences tab
            selectInput("selectColour2", label = "Select plot colour", 
                        choices = c("darkgrey","bisque3","darkseagreen4", "cadetblue", "deepskyblue4"), 
                        selected = "darkseagreen4")
          ),
          
          fluidRow(
            column(12,
                   h5("General coffee habits and preferences"))
          ),
          
          fluidRow(
            column(4,
                   br(),
                   # FEATURE 9: SELECT WHICH COFFEE DRINKING HABITS AND PREFERENCES DATA TO DISPLAY
                   # I added this radio button widget so users can choose which coffee drinking habits and preferences 
                   # data they would like to visualize, rather than cluttering this tab with many individual static plots. 
                   radioButtons("prefPlot", "What would you like to plot?",
                           choices = c(
                             "Cups of coffee per day" = "`How many cups of coffee do you typically drink per day?`", 
                             "Flavour profile preference" = "`Before today's tasting, which of the following best described what kind of coffee you like?`", 
                             "Coffee strength preference" = "`How strong do you like your coffee?`", 
                             "Roast level preference" = "`What roast level of coffee do you prefer?`",
                             "Caffeine level preference" = "`How much caffeine do you like in your coffee?`",
                             "Favorite coffee drink" = "`What is your favorite coffee drink?`"),
                           selected = "`How many cups of coffee do you typically drink per day?`"),
              style = "border:2px solid black"
            ),
            column(8,
                   plotOutput("preference")
            )),
          
          fluidRow(
            column(12,
                   h5("How/where do people acquire their coffee and where do they drink it?"))
          ),
          
          fluidRow(
            column(4,
                   br(),
                   # FEATURE 9 (NEW INSTANCE OF REPEAT FEATURE TYPE): SELECT WHICH COFFEE DRINKING HABITS AND PREFERENCES DATA TO DISPLAY
                   # Same as above, but to display some different data 
                   radioButtons("logPlot", "What would you like to plot?",
                                choices = c(
                                  "Typical purchase location", 
                                  "At-home brewing method", 
                                  "Typical coffee-drinking location"),
                                selected = "Typical purchase location"),
                   style = "border:2px solid black"
            ),
            column(8,
                   plotOutput("logistics")
            )),
          
          fluidRow(
            column(12,
                   h5("What do people add to their coffee?"))
          ),
          
          fluidRow(
            column(4,
                   br(),
                   # FEATURE 9 (NEW INSTANCE OF REPEAT FEATURE TYPE): SELECT WHICH COFFEE DRINKING HABITS AND PREFERENCES DATA TO DISPLAY
                   # Same as above, but to display some different data  
                   radioButtons("additionsPlot", "What would you like to plot?",
                                choices = c(
                                  "General types of add-ins", 
                                  "Dairy add-ins", 
                                  "Sweetener add-ins"),
                                selected = "General types of add-ins"),
                   style = "border:2px solid black"
            ),
            column(8,
                   plotOutput("additions")
            )),
          
          fluidRow(
            h5("Coffee Expertise"),
            p("Participants were also asked about their perceived level of coffee expertise on a scale of 1-10. 
              The results are shown below."),
            plotOutput("expertise")
          )),
        
        # TAB TO DISPLAY RESPONSES FOR QUESTIONS ABOUT MONEY SPENT ON COFFEE-RELATED THINGS
        tabPanel("Coffee-Related Expenses",
                 fluidRow(
                   h3("Money spent on coffee-related items"),
                   p("Participants were asked a few questions about how much money they spend on coffee and coffee-related items. 
                     The responses are plotted below."),
                   # FEATURE 8: SELECT PLOT COLOUR
                   # Same as above, but for coffee expenses tab
                   selectInput("selectColour3", "Select plot colour", 
                               choices = c("darkgrey","bisque3","darkseagreen4", "cadetblue", "deepskyblue4"), 
                               selected = "darkseagreen4"),
                 ),
                 
                 fluidRow(
                   column(4,
                          br(),
                          # FEATURE 10 (NEW): SELECT WHICH COFFEE EXPENSES TO DISPLAY AND APPLY MULTIPLE FILTERS
                          # I added three selectInput features here to allow users to (1) choose what coffee expenses data to plot, 
                          # (2) filter the plotted data based on gender, and (3) filter the plotted data based on age.
                          # This allows users to see how spending habits vary between genders and across age groups.
                          selectInput("expensesPlot1", "What would you like to plot?",
                                       choices = c(
                                         "What is the most you've ever paid for a cup of coffee?" = "`What is the most you've ever paid for a cup of coffee?`", 
                                         "What is the most you'd ever pay for a cup of coffee?" = "`What is the most you'd ever be willing to pay for a cup of coffee?`", 
                                         "How much money have participants spent on coffee equipment in the past 5 yrs?" = "`Approximately how much have you spent on coffee equipment in the past 5 years?`"),
                                       selected = "`What is the most you've ever paid for a cup of coffee?`"),
                          selectInput("expensesGen1",
                                      "Filter data by gender:",
                                      c("All", "Female", "Male", "Non-binary", "Other (please specify)", "Prefer not to say")),
                          selectInput("expensesAge1",
                                      "Filter data by age:",
                                      c("All", "<18 years old", "18-24 years old", "25-34 years old", "35-44 years old", "45-54 years old", "55-64 years old", ">65 years old")),
                          style = "border:2px solid black"
                   ),
                   column(8,
                          plotOutput("expenses1"),
                          br()
                   )),
                 
                 fluidRow(
                   column(12,
                          p("Participants were also asked about their level of satisfaction regarding money spent on coffee and coffee equipment. 
                            The results are summarised in the table below."))
                 ),
                 
                 fluidRow(
                   column(4,
                          # FEATURE 10: SELECT WHICH COFFEE EXPENSE OPINIONS TO DISPLAY AND APPLY MULTIPLE FILTERS
                          # Same as above, but to display some different data
                          selectInput("expensesPlot2", "What would you like to display?",
                                      choices = c(
                                        "Do you feel like you’re getting good value for your money when you buy coffee at a cafe?" = "`Do you feel like you’re getting good value for your money when you buy coffee at a cafe?`", 
                                        "Do you feel like you’re getting good value for your money with regards to your coffee equipment?" = "`Do you feel like you’re getting good value for your money with regards to your coffee equipment?`"),
                                      selected = "`Do you feel like you’re getting good value for your money when you buy coffee at a cafe?`"),
                          selectInput("expensesGen2",
                                      "Filter data by gender:",
                                      c("All", "Female", "Male", "Non-binary", "Other (please specify)", "Prefer not to say")),
                          selectInput("expensesAge2",
                                      "Filter data by age:",
                                      c("All", "<18 years old", "18-24 years old", "25-34 years old", "35-44 years old", "45-54 years old", "55-64 years old", ">65 years old")),
                          style = "border:2px solid black"
                   ),
                   column(8,
                          plotOutput("expenses2")
                   ),
                   br()
                   )
        ),
        
        # TAB TO DISPLAY KEY TASTE TEST RESULTS
        tabPanel("Taste Test",
          fluidRow(
            h3("Taste Test Results"),
            # FEATURE 8: SELECT PLOT COLOUR
            # Same as above, but for taste test results tab
            selectInput("selectColour4", label = "Select plot colour", 
                        choices = c("darkgrey","bisque3","darkseagreen4", "cadetblue", "deepskyblue4"), 
                        selected = "darkseagreen4"),
            # FEATURE 11: FILTER TASTE TEST RESULTS BY GENDER
            # I added a select input widget so the user can filter the plotted taste test results by participant gender.
            # I thought this would be interesting, as James Hoffmann's original video discussing the findings of this taste 
            # test highlighted that male and female participants had quite different preferences between the coffee types
            selectInput("selectGender", label = "The taste test results varied interestingly based on gender.
                                                 Filter the data by participant gender here.", 
                        choices = c("All","Male", "Female", "Other (please specify)", "Non-binary"), 
                        selected = "All"),
            h4("Overall Preference"),
            p("Out of coffees A-D, participants were asked to identify their favourite one. The plot below shows how many 
              participants chose each coffee as their favourite."),
            plotOutput("overallPref"),
            br()
          ),
          
          fluidRow(
            column(6,
                   h4("Roast Preference"),
                   p("Coffees A-C were all washed coffees with different roast levels. Coffee A was a light roast, B was 
                     a medium roast, and C was a dark roast. Out of coffees A-C, participants were asked to identify their 
                     favourite to evaluate roast preference. Results are plotted below."),
                   plotOutput("roastPref")
                   ),
            column(6,
                   h4("Process Preference"),
                   p("Cofees A and D were both light roast coffees, but A was a washed coffee and D was a natural processed 
                     (fermented) coffee. Out of coffees A and D, participants were asked to identify their favourite to 
                     evaluate coffee process preference. Results are plotted below."),
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
  
  # PLOT DEMOGRAPHICS DATA
  output$demographics <- renderPlot({
    ggplot(data, aes_string(input$demoPlot)) +
      geom_bar(fill = input$selectColour1) +
      ylab("Number of participants") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      scale_x_discrete(guide = guide_axis(angle = 45))
  })
  
  # PLOT DATA TABLE OF ALL DATA
  output$all_data <- DT::renderDataTable(DT::datatable({
    df <- data
    if (input$gen != "All") {
      df <- df %>%
        filter(Gender == input$gen)
    }
    if (input$age != "All") {
      df <- df %>%
        filter(`What is your age?` == input$age)
    }
    df
  }))
  
  # PLOT THE COFFEE DRINKING HABITS/PREFERENCE DATA SELECTED BY THE USER
  output$preference <- renderPlot({
    ggplot(data, aes_string(input$prefPlot)) +
      geom_bar(fill = input$selectColour2) +
      ylab("Number of participants") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      scale_x_discrete(guide = guide_axis(angle = 45))
  })
  
  # WHERE/HOW DO PEOPLE ACQUIRE THEIR COFFEE AND WHERE DO THEY DRINK IT?
  # Where do people drink their coffee? - groom data into desired format
  l1 <- data %>%
    group_by(`Where do you typically drink coffee? (At home)`) %>% 
    summarise(`At home` = n())
  l2 <- data %>%
    group_by(`Where do you typically drink coffee? (At the office)`) %>% 
    summarise(`At the office` = n())
  l3 <- data %>%
    group_by(`Where do you typically drink coffee? (On the go)`) %>% 
    summarise(`On the go` = n())
  l4 <- data %>%
    group_by(`Where do you typically drink coffee? (At a cafe)`) %>% 
    summarise(`At a cafe` = n())
  l5 <- data %>%
    group_by(`Where do you typically drink coffee? (None of these)`) %>% 
    summarise(`None of these` = n())
  
  levels_drink <- c("At home", "At the office", "On the go", "At a cafe", "None of these")
  
  coffee_drinking_location <- data.frame(drink_location = factor(c("At home", "At the office", "On the go", "At a cafe", 
                                                             "None of these"), levels_drink),
                                         count_loc = c(l1[2,2][[1]], l2[2,2][[1]], l3[2,2][[1]], l4[2,2][[1]], l5[2,2][[1]]))
  
  # How do you brew coffee at home? - groom data into desired format
  m1 <- data %>%
    group_by(`How do you brew coffee at home? (Pour over)`) %>% 
    summarise(`Pour over` = n())
  m2 <- data %>%
    group_by(`How do you brew coffee at home? (French press)`) %>% 
    summarise(`French press` = n())
  m3 <- data %>%
    group_by(`How do you brew coffee at home? (Espresso)`) %>% 
    summarise(`Espresso` = n())
  m4 <- data %>%
    group_by(`How do you brew coffee at home? (Coffee brewing machine (e.g. Mr. Coffee))`) %>% 
    summarise(`Coffee brewing machine` = n())
  m5 <- data %>%
    group_by(`How do you brew coffee at home? (Pod/capsule machine (e.g. Keurig/Nespresso))`) %>% 
    summarise(`Pod/capsule machine` = n())
  m6 <- data %>%
    group_by(`How do you brew coffee at home? (Instant coffee)`) %>% 
    summarise(`Instant coffee` = n())
  m7 <- data %>%
    group_by(`How do you brew coffee at home? (Bean-to-cup machine)`) %>% 
    summarise(`Bean-to-cup machine` = n())
  m8 <- data %>%
    group_by(`How do you brew coffee at home? (Cold brew)`) %>% 
    summarise(`Cold brew` = n())
  m9 <- data %>%
    group_by(`How do you brew coffee at home? (Coffee extract (e.g. Cometeer))`) %>% 
    summarise(`Coffee extract` = n())
  m10 <- data %>%
    group_by(`How do you brew coffee at home? (Other)`) %>% 
    summarise(`Other` = n())
  
  levels_method <- c("Pour over", "French press", "Espresso", "Coffee brewing machine", "Pod/capsule machine", "Instant coffee",
                     "Bean-to-cup machine", "Cold brew", "Coffee extract", "Other")
  
  brew_method <- data.frame(method = factor(c("Pour over", "French press", "Espresso", "Coffee brewing machine", 
                                              "Pod/capsule machine", "Instant coffee", "Bean-to-cup machine", 
                                              "Cold brew", "Coffee extract", "Other"), levels_method),
                            count_method = c(m1[2,2][[1]], m2[2,2][[1]], m3[2,2][[1]], m4[2,2][[1]], m5[2,2][[1]], m6[2,2][[1]],
                                             m7[2,2][[1]], m8[2,2][[1]], m9[2,2][[1]], m10[2,2][[1]]))
  
  # On the go, where do you typically purchase coffee? - groom data into desired format
  g1 <- data %>%
    group_by(`On the go, where do you typically purchase coffee? (National chain (e.g. Starbucks, Dunkin))`) %>%
    summarise(`National chain` = n())
  g2 <- data %>%
    group_by(`On the go, where do you typically purchase coffee? (Local cafe)`) %>% 
    summarise(`Local cafe` = n())
  g3 <- data %>%
    group_by(`On the go, where do you typically purchase coffee? (Drive-thru)`) %>% 
    summarise(`Drive-thru` = n())
  g4 <- data %>%
    group_by(`On the go, where do you typically purchase coffee? (Specialty coffee shop)`) %>% 
    summarise(`Specialty coffee shop` = n())
  g5 <- data %>%
    group_by(`On the go, where do you typically purchase coffee? (Deli or supermarket)`) %>% 
    summarise(`Deli or supermarket` = n())
  g6 <- data %>%
    group_by(`On the go, where do you typically purchase coffee? (Other)`) %>% 
    summarise(`Other` = n())
  
  levels_otg <- c("National chain", "Local cafe", "Drive-thru", "Specialty coffee shop", "Deli or supermarket", "Other")
  
  on_the_go <- data.frame(otg_location = factor(c("National chain", "Local cafe", "Drive-thru", "Specialty coffee shop", 
                                                  "Deli or supermarket", "Other"), levels_otg),
                          count_otg = c(g1[2,2][[1]], g2[2,2][[1]], g3[2,2][[1]], g4[2,2][[1]], g5[2,2][[1]], g6[2,2][[1]]))
  
  # Make the plot
  output$logistics <- renderPlot({
    if (input$logPlot == "Typical purchase location") {
      on_the_go %>%
        ggplot(aes(x = otg_location, y = count_otg)) +
        geom_col(fill = input$selectColour2) +
        ylab("Number of participants") +
        xlab("On the go, where do you typically purchase coffee?") +
        theme_minimal(base_size = 14) +
        theme(panel.grid.minor = element_blank()) +
        scale_x_discrete(guide = guide_axis(angle = 45))
    }
    else if (input$logPlot == "At-home brewing method") {
      brew_method %>%
        ggplot(aes(x = method, y = count_method)) +
        geom_col(fill = input$selectColour2) +
        ylab("Number of participants") +
        xlab("How do you brew coffee at home?") +
        theme_minimal(base_size = 14) +
        theme(panel.grid.minor = element_blank()) +
        scale_x_discrete(guide = guide_axis(angle = 45))
    }
    else {
      coffee_drinking_location %>%
        ggplot(aes(x = drink_location, y = count_loc)) +
        geom_col(fill = input$selectColour2) +
        ylab("Number of participants") +
        xlab("Where do you typically drink coffee?") +
        theme_minimal(base_size = 14) +
        theme(panel.grid.minor = element_blank()) +
        scale_x_discrete(guide = guide_axis(angle = 45))
    }
  })
  
  # WHAT DO PEOPLE ADD TO THEIR COFFEE?
  # Do you usually add anything to your coffee? - groom data into desired format
  a1 <- data %>%
    group_by(`Do you usually add anything to your coffee? (No - just black)`) %>%
    summarise(`No - just black` = n())
  a2 <- data %>%
    group_by(`Do you usually add anything to your coffee? (Milk, dairy alternative, or coffee creamer)`) %>%
    summarise(`Milk, dairy alternative, or coffee creamer` = n())
  a3 <- data %>%
    group_by(`Do you usually add anything to your coffee? (Sugar or sweetener)`) %>%
    summarise(`Sugar or sweetener` = n())
  a4 <- data %>%
    group_by(`Do you usually add anything to your coffee? (Flavor syrup)`) %>%
    summarise(`Flavor syrup` = n())
  a5 <- data %>%
    group_by(`Do you usually add anything to your coffee? (Other)`) %>%
    summarise(`Other` = n())
  
  levels <- c("No - just black", "Milk, dairy alternative, or coffee creamer", "Sugar or sweetener", "Flavor syrup", "Other")
  
  general_add_ins <- data.frame(add_ins = factor(c("No - just black", "Milk, dairy alternative, or coffee creamer", 
                                             "Sugar or sweetener", "Flavor syrup", "Other"), levels),
                          count = c(a1[2,2][[1]], a2[2,2][[1]], a3[2,2][[1]], a4[2,2][[1]], a5[2,2][[1]]))
  
  # What kind of dairy do you add? - groom data into desired format
  d1 <- data %>%
    group_by(`What kind of dairy do you add? (Whole milk)`) %>%
    summarise(`Whole milk` = n())
  d2 <- data %>%
    group_by(`What kind of dairy do you add? (Skim milk)`) %>%
    summarise(`Skim milk` = n())
  d3 <- data %>%
    group_by(`What kind of dairy do you add? (Half and half)`) %>%
    summarise(`Half and half` = n())
  d4 <- data %>%
    group_by(`What kind of dairy do you add? (Coffee creamer)`) %>%
    summarise(`Coffee creamer` = n())
  d5 <- data %>%
    group_by(`What kind of dairy do you add? (Flavored coffee creamer)`) %>%
    summarise(`Flavored coffee creamer` = n())
  d6 <- data %>%
    group_by(`What kind of dairy do you add? (Oat milk)`) %>%
    summarise(`Oat milk` = n())
  d7 <- data %>%
    group_by(`What kind of dairy do you add? (Almond milk)`) %>%
    summarise(`Almond milk` = n())
  d8 <- data %>%
    group_by(`What kind of dairy do you add? (Soy milk)`) %>%
    summarise(`Soy milk` = n())
  
  levels <- c("Whole milk", "Skim milk", "Half and half", "Coffee creamer", "Flavored coffee creamer", "Oat milk",
              "Almond milk", "Soy milk")
  
  dairy <- data.frame(dairy_add_ins = factor(c("Whole milk", "Skim milk", "Half and half", "Coffee creamer", 
                                               "Flavored coffee creamer", "Oat milk", "Almond milk", "Soy milk"), levels),
                      count = c(d1[2,2][[1]], d2[2,2][[1]], d3[2,2][[1]], d4[2,2][[1]], d5[2,2][[1]], d6[2,2][[1]],
                                d7[2,2][[1]], d8[2,2][[1]]))
  
  # What kind of sugar or sweetener do you add? - groom data into desired format
  s1 <- data %>%
    group_by(`What kind of sugar or sweetener do you add? (Granulated Sugar)`) %>%
    summarise(`Granulated Sugar` = n())
  s2 <- data %>%
    group_by(`What kind of sugar or sweetener do you add? (Artificial Sweeteners (e.g., Splenda))`) %>%
    summarise(`Artificial Sweeteners (e.g., Splenda)` = n())
  s3 <- data %>%
    group_by(`What kind of sugar or sweetener do you add? (Honey)`) %>%
    summarise(`Honey` = n())
  s4 <- data %>%
    group_by(`What kind of sugar or sweetener do you add? (Maple Syrup)`) %>%
    summarise(`Maple Syrup` = n())
  s5 <- data %>%
    group_by(`What kind of sugar or sweetener do you add? (Stevia)`) %>%
    summarise(`Stevia` = n())
  s6 <- data %>%
    group_by(`What kind of sugar or sweetener do you add? (Agave Nectar)`) %>%
    summarise(`Agave Nectar` = n())
  s7 <- data %>%
    group_by(`What kind of sugar or sweetener do you add? (Brown Sugar)`) %>%
    summarise(`Brown Sugar` = n())
  s8 <- data %>%
    group_by(`What kind of sugar or sweetener do you add? (Raw Sugar (Turbinado))`) %>%
    summarise(`Raw Sugar (Turbinado)` = n())
  
  levels <- c("Graulated Sugar", "Artificial Sweeteners (e.g., Splenda)", "Honey", "Maple Syrup", "Stevia", "Agave Nectar",
              "Brown Sugar", "Raw Sugar (Turbinado)")
  
  sweetener <- data.frame(sweetener_add_ins = factor(c("Graulated Sugar", "Artificial Sweeteners (e.g., Splenda)", "Honey", 
                                                       "Maple Syrup", "Stevia", "Agave Nectar", "Brown Sugar", 
                                                       "Raw Sugar (Turbinado)"), levels),
                          count = c(s1[2,2][[1]], s2[2,2][[1]], s3[2,2][[1]], s4[2,2][[1]], s5[2,2][[1]], s6[2,2][[1]],
                                    s7[2,2][[1]], s8[2,2][[1]]))
  
  # Make the plot
  output$additions <- renderPlot({
    if (input$additionsPlot == "General types of add-ins") {
      general_add_ins %>%
        ggplot(aes(x = add_ins, y = count)) +
        geom_col(fill = input$selectColour2) +
        ylab("Number of participants") +
        xlab("Do you usually add anything to your coffee?") +
        theme_minimal(base_size = 14) +
        theme(panel.grid.minor = element_blank()) +
        scale_x_discrete(guide = guide_axis(angle = 45))
    }
    else if (input$additionsPlot == "Dairy add-ins") {
      dairy %>%
        ggplot(aes(x = dairy_add_ins, y = count)) +
        geom_col(fill = input$selectColour2) +
        ylab("Number of participants") +
        xlab("What kind of dairy do you add?") +
        theme_minimal(base_size = 14) +
        theme(panel.grid.minor = element_blank()) +
        scale_x_discrete(guide = guide_axis(angle = 45))
    }
    else {
      sweetener %>%
        ggplot(aes(x = sweetener_add_ins, y = count)) +
        geom_col(fill = input$selectColour2) +
        ylab("Number of participants") +
        xlab("What kind of sugar or sweetener do you add?") +
        theme_minimal(base_size = 14) +
        theme(panel.grid.minor = element_blank()) +
        scale_x_discrete(guide = guide_axis(angle = 45))
    }
  })
  
  # PLOT PARTICIPANTS' SELF-REPORTED COFFEE EXPERTISE
  output$expertise <- renderPlot({
    ggplot(data = subset(data, !is.na(`Lastly, how would you rate your own coffee expertise?`)), 
           aes(`Lastly, how would you rate your own coffee expertise?`, na.rm = TRUE)) +
      geom_bar(fill = input$selectColour2) +
      ylab("Number of participants") +
      xlab("Level of coffee expertise") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) 
  })
  
  # PLOT THE COFFEE EXPENSES DATA SELECTED BY THE USER
  data_expenses1 <- reactive({
    df1 <- data
    if (input$expensesGen1 != "All") {
      df1 <- df1 %>%
        filter(Gender == input$expensesGen1)
    }
    if (input$expensesAge1 != "All") {
      df1 <- df1 %>%
        filter(`What is your age?` == input$expensesAge1)
    }
    df1
  })

  output$expenses1 <- renderPlot({
    ggplot(data_expenses1(), aes_string(input$expensesPlot1)) +
      geom_bar(fill = input$selectColour3) +
      ylab("Number of participants") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      scale_x_discrete(guide = guide_axis(angle = 45))
  })
  
  # PLOT THE COFFEE EXPENSES OPINIONS DATA SELECTED BY THE USER
  data_expenses2 <- reactive({
    df2 <- data
    if (input$expensesGen2 != "All") {
      df2 <- df2 %>%
        filter(Gender == input$expensesGen2)
    }
    if (input$expensesAge2 != "All") {
      df2 <- df2 %>%
        filter(`What is your age?` == input$expensesAge2)
    }
    df2
  })
  
  output$expenses2 <- renderPlot({
    ggplot(data_expenses2(), aes_string(input$expensesPlot2)) +
      geom_bar(fill = input$selectColour3) +
      ylab("Number of participants") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      scale_x_discrete(guide = guide_axis(angle = 45))
  })
  
  # FILTER THE TASTE TEST RESULT DATA BY GENDER, IF SELECTED BY USER
  data_taste <- reactive({
    if (input$selectGender == "All"){
      data
    }
    else {
      data %>%
        filter(Gender == input$selectGender)
    }
  })
  
  # PLOT OVERALL COFFEE PREFERENCE RESULTS FROM TASTE TEST
  output$overallPref <- renderPlot({
    ggplot(data = subset(data_taste(), !is.na(`Lastly, what was your favorite overall coffee?`)), 
           aes(`Lastly, what was your favorite overall coffee?`)) +
      geom_bar(fill = input$selectColour4) +
      ylab("Number of participants") +
      xlab("Favourite overall coffee") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      scale_x_discrete(guide = guide_axis(angle = 45))
  })
  
  #PLOT COFFEE ROAST PREFERENCE RESULTS FROM TASTE TEST
  output$roastPref <- renderPlot({
    ggplot(data = subset(data_taste(), !is.na(`Between Coffee A, Coffee B, and Coffee C which did you prefer?`)), 
           aes(`Between Coffee A, Coffee B, and Coffee C which did you prefer?`)) +
      geom_bar(fill = input$selectColour4) +
      ylab("Number of participants") +
      xlab("Roast preference") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      scale_x_discrete(guide = guide_axis(angle = 45))
  })
  
  #PLOT COFFEE PROCESS PREFERENCE RESULTS FROM TASTE TEST
  output$processPref <- renderPlot({
    ggplot(data = subset(data_taste(), !is.na(`Between Coffee A and Coffee D, which did you prefer?`)), 
           aes(`Between Coffee A and Coffee D, which did you prefer?`)) +
      geom_bar(fill = input$selectColour4) +
      ylab("Number of participants") +
      xlab("Process preference") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      scale_x_discrete(guide = guide_axis(angle = 45))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
