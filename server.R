library(ggplot2)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)


df <- read.csv("adult_census_income.csv") %>%
  mutate(education = factor(education,
                            levels=c("Preschool","1st-4th","5th-6th","7th-8th","9th","10th","11th","12th","HS-grad","Some-college","Assoc-voc","Assoc-acdm","Bachelors","Masters","Prof-school","Doctorate"),
                            ordered=TRUE))

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

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  #Welcome Screen

  output$data_text <- renderText("These data come from a subset of the 1994 US census. The data were obtained by Ronny Kohavi and Barry Becker who used them to predict a persons income class: either above or below $50,000 per year. See https://www.kaggle.com/uciml/adult-census-income for the dataset and more information about the original problem.")
  
  # Ross Graphs
  
  output$inc_plot <- renderPlotly({
    census_subset <- census
    if (!input$inc0) {
      census_subset <- census_subset %>% filter(capital.income != 0)
    }
    if (!input$inc99999) {
      census_subset <- census_subset %>% filter(capital.income != 99999)
    }
    if (input$occupation != "all") {
      census_subset <- census_subset %>% filter(occupation == input$occupation)
    }
    if (input$workclass != "all") {
      census_subset <- census_subset %>% filter(workclass == input$workclass)
    }
    
    ci_plot <- ggplot(data = census_subset, aes(x = capital.income, 
                                                fill = workclass)) +
      labs(title = "Distribution of Capital Income",
           subtitle = "The Intersection of Occupation and Work Sector",
           x = "Capital Income")
    if (input$prop_hist) {
      ci_plot <- ci_plot + 
        geom_histogram(binwidth = 1000, 
                       aes(y = ..count.. /
                             tapply(..count.., ..PANEL.., sum)[..PANEL..])) +
        labs(y = "Proportion")
    }
    else {
      ci_plot <- ci_plot + geom_histogram(binwidth = 1000) +
        labs(y = "Count")
    }
    if (input$workclass == "all"){
      ci_plot <- ci_plot + labs(fill = "Work Sector")
    }
    else{
      ci_plot <- ci_plot + guides(fill = FALSE)
    }
    if (input$facet == "sex") {
      ci_plot <- ci_plot + facet_grid(sex ~ .)
    }
    if (input$facet == "race") {
      ci_plot <- ci_plot + facet_wrap(~race)
    }
    if (input$facet == "us_born") {
      ci_plot <- ci_plot + facet_grid(us_born ~ .)
    }
    ci_plot +
      scale_fill_brewer(palette = "Set3")
  })
  
  output$cat_plot <- renderPlot({
    census_subset <- census
    if (!input$inc0) {
      census_subset <- census_subset %>% filter(capital.income != 0)
    }
    if (!input$inc99999) {
      census_subset <- census_subset %>% filter(capital.income != 99999)
    }
    if (input$occupation != "all") {
      census_subset <- census_subset %>% filter(occupation == input$occupation)
    }
    if (input$workclass != "all") {
      census_subset <- census_subset %>% filter(workclass == input$workclass)
    }
    ct_p <- 
      ggplot(census_subset, aes(x = workclass, fill = income)) +
      geom_bar(position = "fill") +
      coord_flip() + 
      labs(title = "Relationship between Income Category and Work Sector",
           y = "Proportion",
           x = "Work Sector",
           fill = "Income")
    if (input$facet == "sex") {
      ct_p <- ct_p + facet_grid(sex ~ .)
    }
    if (input$facet == "race") {
      ct_p <- ct_p + facet_wrap(~race)
    }
    if (input$facet == "us_born") {
      ct_p <- ct_p + facet_grid(us_born ~ .)
    }
    ct_p +
      scale_fill_brewer(palette = "Set3")
  })
  
  # Devansh Graphs
  output$dk_census_plot_1 <- renderPlot({
    ## PUT YOUR GGPLOT CODE HERE ##
    df_cur = df
    if(!input$edges && input$metric == 'Capital Gains') df_cur <- filter(df, capital.gain < 99999 & capital.gain > 0)
    if(!input$edges && input$metric == 'Capital Losses') df_cur <- filter(df, capital.loss > 0)
    
    title = "Education Level and Investment Success"
    
    ggplot(df_cur, aes_string(x='education', y = ifelse(input$metric == 'Capital Gains', "capital.gain", "capital.loss"))) +
      geom_boxplot(alpha=0.7) +
      geom_point(alpha=0.3) +
      geom_jitter(width=0.1, height=0.1) +
      geom_smooth(aes(x=education.num)) +
      labs(title=title,
           x="Highest Education Level",
           y=input$metric)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  output$dk_census_plot_2 <- renderPlot({
    ## PUT YOUR GGPLOT CODE HERE ##
    df_cur = df
    if(input$sex != 'Both') df_cur <- filter(df, sex == input$sex)
    if(input$race != 'All') df_cur <- filter(df_cur, race == input$race)
    
    title = sprintf("Education Level and Gender on Income (%d entries)", nrow(df_cur))
    
    ggplot(df_cur, aes_string(x='education', fill = 'income')) +
      geom_bar(position='fill') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = title,
           x="Highest Education Level",
           y="Proportion") +
      scale_fill_brewer(palette = "Set3")
  })
  
  output$oliver_plot_1 <- renderPlot({
    
    # data pre-processing for oliver-specific graphs
    census <- read.csv("adult_census_income.csv")
    nan_idx <- union(which(census$occupation == '?'), which(census$workclass == '?'))
    census_light <- df[-nan_idx,]
    unisex.relationship <- census_light$relationship
    levels(unisex.relationship) <- c(levels(unisex.relationship),"Married")
    unisex.relationship[which(unisex.relationship == "Husband" | unisex.relationship == "Wife")] = "Married"
    census_light <- mutate(census_light,
                           unisex.relationship = unisex.relationship)
    
    education_abs <- census_light$education
    levels(education_abs) <- c(levels(education_abs), "Elementary", "High-school", "Associate")
    education_abs[which(education_abs == "1st-4th" | education_abs == "5th-6th")] = "Elementary"
    education_abs[which(education_abs == "7th-8th" | education_abs == "9th" |
                          education_abs == "10th" | education_abs == "11th" |
                          education_abs == "12th" | education_abs == "HS-grad")] = "High-school"
    education_abs[which(education_abs == "Assoc-acdm" | education_abs == "Assoc-voc")] = "Associate"
    census_light <- mutate(census_light,
                           education_abs = education_abs)
    
    hours.discrete <-c("0-29 hrs", "30-39 hrs", "40-49 hrs", "50-59 hrs", ">60 hrs")[
      findInterval(census_light$hours.per.week , c(-Inf, 30, 40, 50, 60, Inf) )]
    census_light <- mutate(census_light,
                           hours.discrete = hours.discrete)
    #preprocessing ends
    df <- filter(census_light, education_abs == input$education)
    print(input$education)
    
    p <- ggplot(data = df, aes_string(x = "unisex.relationship", fill = factor(df$hours.discrete,
                                                                               levels = c("0-29 hrs", "30-39 hrs", "40-49 hrs", "50-59 hrs", ">60 hrs")))) +
      geom_bar(position = "fill") +
      labs(x = "Relationship",
           y = "Proportion",
           fill = "Working Hours\nper Week",
           title = "Working Hours by Relationship and Education Level") +
      scale_fill_brewer(palette = "Set3")
    p
  })
  
  output$oliver_plot_2 <- renderPlot({
    # data pre-processing for oliver-specific graphs
    census <- read.csv("adult_census_income.csv")
    nan_idx <- union(which(census$occupation == '?'), which(census$workclass == '?'))
    census_light <- df[-nan_idx,]
    unisex.relationship <- census_light$relationship
    levels(unisex.relationship) <- c(levels(unisex.relationship),"Married")
    unisex.relationship[which(unisex.relationship == "Husband" | unisex.relationship == "Wife")] = "Married"
    census_light <- mutate(census_light,
                           unisex.relationship = unisex.relationship)
    
    education_abs <- census_light$education
    levels(education_abs) <- c(levels(education_abs), "Elementary", "High-school", "Associate")
    education_abs[which(education_abs == "1st-4th" | education_abs == "5th-6th")] = "Elementary"
    education_abs[which(education_abs == "7th-8th" | education_abs == "9th" |
                          education_abs == "10th" | education_abs == "11th" |
                          education_abs == "12th" | education_abs == "HS-grad")] = "High-school"
    education_abs[which(education_abs == "Assoc-acdm" | education_abs == "Assoc-voc")] = "Associate"
    census_light <- mutate(census_light,
                           education_abs = education_abs)
    
    hours.discrete <-c("0-29 hrs", "30-39 hrs", "40-49 hrs", "50-59 hrs", ">60 hrs")[
      findInterval(census_light$hours.per.week , c(-Inf, 30, 40, 50, 60, Inf) )]
    census_light <- mutate(census_light,
                           hours.discrete = hours.discrete)
    #preprocessing ends
    df <- filter(census_light, unisex.relationship == input$relationship)
    df <- df[-which(df$occupation == "Armed-Forces" |
                      df$occupation == "Tech-support" |
                      df$occupation == "Protective-serv" |
                      df$occupation == "Farming-fishing" |
                      df$occupation == "Priv-house-serv" |
                      df$occupation == "Handlers-cleaners"),]
    
    p <- ggplot(data = df, aes_string(x = factor(1), fill = "occupation")) +
      geom_bar(position = "fill", width = 1) +
      facet_wrap(~ sex) +
      labs(x = "",
           y = "",
           fill = "Occupation",
           title = "Distribution of Occupation by Gender and Relationship") +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3")
    p
    
  })
  
}
