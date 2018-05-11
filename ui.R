library(ggplot2)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)

df <- read.csv("adult_census_income.csv")
df <- mutate(df,
             education = factor(education,
                                levels=c("Preschool","1st-4th","5th-6th","7th-8th","9th","10th","11th","12th","HS-grad","Some-college","Assoc-voc","Assoc-acdm","Bachelors","Masters","Prof-school","Doctorate"),
                                ordered=TRUE))

census <- read_csv("adult_census_income.csv") %>% 
  select(-fnlwgt) %>% 
  mutate(occupation = ifelse(occupation == "?", "Unknown", occupation) %>% 
           factor,
         workclass =  ifelse(workclass == "?", "Unknown", workclass) %>% 
           factor,
         education = education %>% factor(levels=c("Preschool","1st-4th","5th-6th","7th-8th","9th","10th","11th","12th","HS-grad","Some-college","Assoc-voc","Assoc-acdm","Bachelors","Masters","Prof-school","Doctorate"),
                                          ordered=TRUE), 
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
              fluidRow(
                h2("Capital Gains and Education")),
              
              fluidRow(infoBox("Description", "Here we explore the relationship between education and success in the investment world. Capital gains are the amount of money made through investing and losses is the amount of money lost. You'll notice a spike in investment gains after achieving a bachelor's degree.",
                               color = "teal", width=10)),
              fluidRow(
                box(
                  title="Controls",
                  width=12,
                  radioButtons("metric", label='Metric:',
                               choices= c("Capital Gains", "Capital Losses")),
                  
                  checkboxInput("edges", label='Show Edge Entries (0 and 99999):')
                )),
              
              fluidRow(
                box(plotOutput("dk_census_plot_1"),width=12)
              )
      ),
      
      # Third tab content
      tabItem(tabName = "devansh_2",
              fluidRow(h2("Education and Demographics")),
              
              fluidRow(infoBox("Description", "Here we explore the influences of education, race, and gender on income breakdown. We once notice a spike after achieving a bachelor's degree. Also notice that the income breakdown differs between the different genders - namely with women having lower percentages across the board of income >50K.",
                               color = "teal", width= 10)),
              
              fluidRow(
                box(
                  title="Controls",
                  width=2,
                  radioButtons("sex", label='Gender:',
                               choices = c("Both", "Male", "Female")),
                  
                  radioButtons("race", label='Race:',
                               choices = c("All","White", "Black", "Asian-Pac-Islander", "Amer-Indian-Eskimo", "Other"))
                ),
                
                box(plotOutput("dk_census_plot_2"), width=10)
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "oliver_1",
              h2("Relationship, Education, and Workload"),
              
              box(
                title="Controls",
                selectInput("education", label = "Education Level",
                            choices = c("Pre-school" = "Preschool",
                                        "Elementary" = "Elementary",
                                        "High School" = "High-school",
                                        "Associate" = "Associate",
                                        "College" = "Some-college",
                                        "Bachelor" = "Bachelors",
                                        "Master" = "Masters",
                                        "Doctorate" = "Doctorate"))
              ),
              box(plotOutput("oliver_plot_1")),
              infoBox("Description", "In this plot, we explore the associations between relationship, education, and workload.
                      When choose a certain education level, the user is shown the proportional barplot of discretized working
                      hours per week conditioned on relationship status",
                      color = "yellow")
      ),
      
      # Fifth tab content
      tabItem(tabName = "oliver_2",
              h2("Relationship and Occupation"),
              
              box(
                title="Controls",
                selectInput("relationship", label = "Relationship Status",
                            choices = c("Married" = "Married",
                                        "Not in Family" = "Not-in-family",
                                        "Other Relative" = "Other-relative",
                                        "Own Child" = "Own-child",
                                        "Unmarried" = "Unmarried"))
              ),
              box(plotOutput("oliver_plot_2")),
              infoBox("Description", "When a user chooses an relationship status, he/she is shown
                      piecharts examining associations the distribution of occupation conditioning
                      on gender and said relationship",
                      color = "yellow")
      )
      
    )
  )
)
