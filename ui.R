#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyr)
library(shinycssloaders)
library(DT)
library(rsconnect)
data_pred3 = read.csv('data_pred.csv')
data_pred3_p = read.csv('data_pred_pitch.csv')
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = shinytheme('cerulean'),
  
  # Application title
  titlePanel("BayesBall"),
  
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId="pos", label="Hitters/Pitchers:", 
                   choices=c("Hitters", "Pitchers")),
      
      radioButtons(inputId="prorated", label="Prorated (600 PA):", 
                   choices=c("No", "Yes")),
      
      radioButtons(inputId="comp", label="Select Baseline:", 
                   choices=c("None", "Display League Average (2018)", "Compare Two Players")),
      
      conditionalPanel(
        condition = "input.pos == 'Hitters'",
        selectInput(
          inputId = "name",
          label = "Player:",
          choices = data_pred3[order(data_pred3$Name),]$Name
        )
      ),
      conditionalPanel(
        condition = "input.pos == 'Pitchers'",
        selectInput(
          inputId = "name_p",
          label = "Player:",
          choices = data_pred3_p[order(data_pred3_p$Name),]$Name
        )
      ),
      conditionalPanel(
        condition = "input.pos == 'Hitters'",
        selectInput(
          inputId = "stat",
          label = "Statistic:",
          choices = c('PA', 'AB', 'H', 'x1B', 'x2B', 'x3B', 'HR', 'K', 'BB', 'SB', 'AVG', 
                      'OBP', 'SLG', 'OPS', 'wOBA', 'BatWAR', 'Sharpe')
        )
      ),
      conditionalPanel(
        condition = "input.pos == 'Pitchers'",
        selectInput(
          inputId = "stat_p",
          label = "Statistic:",
          choices = c('W', 'L', 'ERA', 'G', 'SV', 'IP', 'H', 'ER', 'HR', 'K', 'BB',
                      'WHIP', 'K9', 'BB9', 'FIP', 'WAR', 'Sharpe')
        )
      ),
      conditionalPanel(
        condition = "input.comp == 'Compare Two Players' && input.pos == 'Hitters'",
        selectInput(inputId = "name2",
                    label = "Player 2:",
                    choices = data_pred3[order(data_pred3$Name),]$Name)
      ),
      conditionalPanel(
        condition = "input.comp == 'Compare Two Players' && input.pos == 'Pitchers'",
        selectInput(inputId = "name2_p",
                    label = "Player 2:",
                    choices = data_pred3_p[order(data_pred3_p$Name),]$Name)
      )
      ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(

      tabsetPanel(type = "tabs",
                  tabPanel("Projections", plotOutput("distPlot") %>% withSpinner(),
                           h2('Posterior Predictive Distribution Data', align = 'center'),
                           DT::dataTableOutput("mytable")),
                  tabPanel("Leaderboard", 
                           h2('Leaderboard', align = 'center'),
                           radioButtons(inputId = 'leaderboard',
                                       label = 'Sort By:',
                                       choices = c('Upside','Bust Potential',
                                                   'High Variance', 'Low Variance')),
                           DT::dataTableOutput('tab1')),
                  tabPanel("Methodology", tags$br(), uiOutput("method")),
                  tabPanel("About", tags$br(), img(src = "me.png", height = 185, style="display: block; margin-left: auto; margin-right: auto;"), 
                           uiOutput("about"))
      )
    )
  ))
)
