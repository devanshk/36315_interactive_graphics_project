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



server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  #Welcome Screen
  
  output$welcome_text <- renderText("We are so sorry you had to see this")
  
  # Ross Graphs
  output$cat_inc_explanation <- renderText("How does one's income vary based on their chosen career path? Do particular occupations pay more or less depending on the sector in which one works? What occupations or work sectors make or lose money through investments? The following graphs allow you to explore different combinations of occupations, work sectors, and demographics to see the association between these factors, income level and capital gains or losses.")
  
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
    ci_plot
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
    ct_p
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
           y="Proportion")
  })
  
  output$oliver_plot_1 <- renderPlot({
    census <- read.csv("adult_census_income.csv")
    nan_idx <- union(which(census$occupation == '?'), which(census$workclass == '?'))
    census_light <- census[-nan_idx,]
    unisex.relationship <- census_light$relationship
    levels(unisex.relationship) <- c(levels(unisex.relationship),"Married")
    unisex.relationship[which(unisex.relationship == "Husband" | unisex.relationship == "Wife")] = "Married"
    census_light <- mutate(census_light,
                           unisex.relationship = unisex.relationship)
    
    p <- ggplot(census_light, aes_string(x = "unisex.relationship", fill = input$which_variable)) +
      geom_bar(position = "fill") +
      labs(x = "relationship",
           title = paste("Relationship vs", input$which_variable))
    
    if(input$do_facet) {
      p <- p + facet_wrap(~ sex)
    }
    
    p
  })
  
  output$oliver_plot_2 <- renderPlot({
    census <- read.csv("adult_census_income.csv")
    
    nan_idx <- union(which(census$occupation == '?'), which(census$workclass == '?'))
    census_light <- census[-nan_idx,]
    unisex.relationship <- census_light$relationship
    levels(unisex.relationship) <- c(levels(unisex.relationship),"Married")
    unisex.relationship[which(unisex.relationship == "Husband" | unisex.relationship == "Wife")] = "Married"
    census_light <- mutate(census_light,
                           unisex.relationship = unisex.relationship)
    
    p <- ggplot(census_light, aes_string(x = "unisex.relationship", fill = input$which_variable_2)) +
      geom_bar(position = "fill") +
      labs(x = "relationship",
           title = paste("Relationship vs", input$which_variable_2))
    
    p
    
  })
  
}
