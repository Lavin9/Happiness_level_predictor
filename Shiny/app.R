rm(list=ls())
library(shiny)
library(readr)
library(dplyr)
raw_data <- read_csv("../2015.csv")
raw_data <-  arrange(raw_data, desc(Country))
dataset1 <- raw_data %>% select (Region, 'Happiness Score', 'Economy (GDP per Capita)', Family, 'Health (Life Expectancy)', Freedom, 'Trust (Government Corruption)', Generosity)
dataset1 <- dataset1 %>% rename(Happiness_score = 'Happiness Score')
dataset1 <- dataset1 %>% rename(GDP_per_Capita = 'Economy (GDP per Capita)')
dataset1 <- dataset1 %>% rename(Life_expectancy = 'Health (Life Expectancy)')
dataset1 <- dataset1 %>% rename(Trust_in_Government = 'Trust (Government Corruption)')
dataset1$Region <- factor(dataset1$Region)
trainingData <- dataset1[1:126,]
LM1 <- lm(Happiness_score ~ GDP_per_Capita + Family + Life_expectancy + Freedom + Trust_in_Government + Generosity + Region, trainingData)

ui <- fluidPage(
  titlePanel("Predict happiness score"),

  sidebarLayout(
    sidebarPanel(
      sidebarLayout(
        sidebarPanel(
  textInput("title1", label = "put GDP score here", placeholder = "0"),
  textInput("title2", label = "put Family score here", placeholder = "0"),
  textInput("title3", label = "put Life expectancy score here", placeholder = "0"),
  textInput("title4", label = "put Freedom score here", placeholder = "0"),width = 5
  ),

  mainPanel(
  textInput("title5", label = "put Trust score here", placeholder = "0"),
  textInput("title6", label = "put Generosity score here", placeholder = "0"),
  textInput("title7", label = "put Region name here", placeholder = "Central and Eastern Europe"),
  actionButton("Start", label = "Go"), width = 5)), width = 9),


  mainPanel(textOutput('result')))
)

server <- function(input, output) {

  string1 <- eventReactive(input$Start, predict(LM1, list(GDP_per_Capita = as.numeric(input$title1),Family = as.numeric(input$title2), Life_expectancy = as.numeric(input$title3), Freedom = as.numeric(input$title4), Trust_in_Government = as.numeric(input$title5), Generosity = as.numeric(input$title6), Region =input$title7)))

  output$result <- renderPrint({
    cat('The happiness level is: ',string1())})
}

shinyApp(ui = ui, server = server)

