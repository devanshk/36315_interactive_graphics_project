library(ggplot2)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)

df <- read.csv("adult_census_income.csv")

census <- read_csv("adult_census_income.csv") %>% 
  select(-fnlwgt) %>% 
  mutate(occupation = ifelse(occupation == "?", "Unknown", occupation) %>% 
           factor,
         workclass =  ifelse(workclass == "?", "Unknown", workclass) %>% 
           factor,
         education = education %>% factor, 
         marital.status = marital.status %>% factor,
         relationship = relationship %>% factor,
         race = race %>% factor,
         sex = sex %>% factor,
         native.country = native.country %>% factor,
         income = income %>% factor,
         capital.income = ifelse(capital.gain > 0, capital.gain, -capital.loss),
         us_born = ifelse(native.country == "United-States", "US Born", "Not US Born"))


ui <- dashboardPage(
  dashboardHeader(title = "Census Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome_screen", icon = icon("chart")),
      menuItem("Career Path and Income", tabName = "ross", icon = icon("building")),
      menuItem("Capital Gains and Education", tabName = "devansh_1", icon = icon("graduation-cap")),
      menuItem("Demographics and Income", tabName = "devansh_2", icon = icon("globe")),
      menuItem("Relationships", tabName = "oliver_1", icon = icon("heart")),
      menuItem("Race", tabName = "oliver_2", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      #Welcome Screen
      tabItem(tabName = "welcome_screen",
              fluidRow(
                h2("An Analysis of 1994 Census Data"),
                box(textOutput("welcome_text")),
                box(textOutput("data_text"))
              )),
      # First tab content
      tabItem(tabName = "ross",
              fluidRow(
                h2("Careers, Work Sector, and Income"),
                
                box(
                  width = 4,
                  title = "Controls",
                  # Input
                  selectInput(
                    inputId = "occupation",
                    label = "Select an Occupation",
                    choices = c("All" = "all",
                                "Administrative/Clerical" = "Adm-clerical", 
                                "Armed Forces" = "Armed-Forces", 
                                "Craft/Repair" = "Craft-repair",
                                "Executive/Managerial" = "Exec-managerial",
                                "Farming/Fishing" = "Farming-fishing",
                                "Handlers/Cleaners" = "Handlers-cleaners",
                                "Machine Operators/Inspectors" = 
                                  "Machine-op-inspct",
                                "Other Services" = "Other-service",
                                "Private House Service" = "Priv-house-serv",
                                "Professional/Specialty" = "Prof-specialty",
                                "Protective Service" = "Protective-serv",
                                "Sales" = "Sales",
                                "Tech Support" = "Tech-support",
                                "Transport/Moving" = "Transport-moving",
                                "Unknown" = "Unknown")),
                  radioButtons("workclass", label = "Select a Work Sector",
                               choices = c("All" = "all",
                                           "Federal Government" = "Federal-gov",
                                           "Local Government" = "Local-gov",
                                           "Never Worked" = "Never-worked",
                                           "Private" = "Private",
                                           "Self Employed (incorporated)" = "Self-emp-inc",
                                           "Self Employed (not incorporated)" = 
                                             "Self-emp-not-inc",
                                           "State Government" = "State-gov",
                                           "Unknown" = "Unknown",
                                           "Without Pay" = "Without-pay")
                  ),
                  checkboxInput("inc0", label = "Include 0 Capital Income?"),
                  checkboxInput("inc99999", label = "Include Capital Income > $99,999?"),
                  checkboxInput("prop_hist", label = "Proportional Histogram?"),
                  radioButtons("facet", label = "Facet?",
                               choices = c("None" = "none",
                                           "Sex" = "sex",
                                           "Race" = "race",
                                           "US Born" = "us_born"))
                ),
                box(textOutput("cat_inc_explanation")),
                box(plotOutput("cat_plot", width = "100%"), width = 8),
                box(plotlyOutput("inc_plot", width = "100%"), width = 12)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "devansh_1",
              h2("Capital Gains and Education"),
              
              box(plotOutput("dk_census_plot_1", width="100%")),
              
              box(
                title="Controls",
                radioButtons("metric", label='Metric:',
                             choices= c("Capital Gains", "Capital Losses")),
                
                checkboxInput("edges", label='Show Edge Entries (0 and 99999):')
              )
              
      ),
      
      # Third tab content
      tabItem(tabName = "devansh_2",
              h2("Education and Demographics"),
              
              box(plotOutput("dk_census_plot_2", width="100%")),
              
              box(
                title="Controls",
                radioButtons("sex", label='Gender:',
                             choices = c("Both", "Male", "Female")),
                
                radioButtons("race", label='Race:',
                             choices = c("All","White", "Black", "Asian-Pac-Islander", "Amer-Indian-Eskimo", "Other"))
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "oliver_1",
              h2("Personal Relationships"),
              
              box(
                title="Controls",
                checkboxInput("do_facet", label = "Facet on Gender"),
                
                radioButtons("which_variable", label = "Which variable?",
                             choices = c("Occupation" = "occupation",
                                         "Marital Status" = "marital.status",
                                         "Work Class" = "workclass",
                                         "Education" = "education",
                                         "Race" = "race"))
              ),
              box(plotOutput("oliver_plot_1"))
      ),
      
      # Fifth tab content
      tabItem(tabName = "oliver_2",
              h2("Race and Work"),
              
              box(
                title="Controls",
                radioButtons("which_variable_2", label = "Which variable?",
                             choices = c("Occupation" = "occupation",
                                         "Marital Status" = "marital.status",
                                         "Work Class" = "workclass",
                                         "Education" = "education")),
                
                radioButtons("which_race_2", label = "Which Race?",
                             choices = c("American-Indian-Eskimo" = "Amer-Indian-Eskimo",
                                         "Asian-Pacific Islander" = "Asian-Pac-Islander",
                                         "Black" = "Black",
                                         "Other" = "Other",
                                         "White" = "White"))
              ),
              box(plotOutput("oliver_plot_2"))
      )
      
    )
  )
)
