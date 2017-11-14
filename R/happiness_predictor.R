library(readr)
library(dplyr)
raw_data <- read_csv("2015.csv")
raw_data <-  arrange(raw_data, desc(Country))
dataset1 <- raw_data %>% select (Region, 'Happiness Score', 'Economy (GDP per Capita)', Family, 'Health (Life Expectancy)', Freedom, 'Trust (Government Corruption)', Generosity)
dataset1 <- dataset1 %>% rename(Happiness_score = 'Happiness Score')
dataset1 <- dataset1 %>% rename(GDP_per_Capita = 'Economy (GDP per Capita)')
dataset1 <- dataset1 %>% rename(Life_expectancy = 'Health (Life Expectancy)')
dataset1 <- dataset1 %>% rename(Trust_in_Government = 'Trust (Government Corruption)')
dataset1$Region <- factor(dataset1$Region)
trainingData <- dataset1[1:126,]
LM1 <- lm(Happiness_score ~ GDP_per_Capita + Family + Life_expectancy + Freedom + Trust_in_Government + Generosity + Region, trainingData)

#' The prediction function for happiness level
#'
#' @param a1 GDP_per_capital score, the extent to which a country's GDP per capita is above the world's lowest national average
#' @param a2 Family score, the extent to which a country's social support is above the world's lowest national average
#' @param a3 Life expectancy score, the extent to which a country's life expectancy is above the world's lowest national average
#' @param a4 Freedom score, the extent to which a country's level of freedom is above the world's lowest national average
#' @param a5 Trust in government score, the extent to which a country's trust in government is above the world's lowest national average
#' @param a6 Generosity score, the extent to which a country's generosity level is above the world's lowest national average
#' @param a7 Region that the country belongs to
#'
#' @examples
#'  \dontrun{
#' happiness_predictor(1.03192,1.23289,0.73608,0.37938,0.19090,0.11046,'Central and Eastern Europe')
#' }
#'
#'
  happiness_predictor <- function(a1,a2,a3,a4,a5,a6,a7){
  happiness_score <- predict(LM1, list(GDP_per_Capita = a1,Family = a2, Life_expectancy = a3, Freedom = a4, Trust_in_Government = a5, Generosity = a6, Region =a7))
  cat("The happiness level is: ", happiness_score)
  }




