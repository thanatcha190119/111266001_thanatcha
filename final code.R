install.packages("readr")
library(readr)
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
library(ggplot2)
setwd("/Users/macbook/Documents/Thesis/HW2/Homework2")
#'1'read data
Homework2_fatalities_data <- read.csv("earthquakes.csv", header = TRUE, sep = ",")
Homework2_urbanpopulation_data <- read.csv("urbanpopulation2.csv", header = TRUE, sep = ",")
#examine data
summary(Homework2_fatalities_data)
summary(Homework2_urbanpopulation_data)
#'3'create sample for analysis
#filter command
Homework2_fatalities_data <- filter(Homework2_fatalities_data, Year >= 1960 & Year <= 2019)
#select command
data_1 <-select(Homework2_fatalities_data, Year, Deaths, Mag, Location.Name, Focal.Depth..km.)
data_2 <-select(Homework2_urbanpopulation_data, year, Urban, urbanpopulation, agland)
#replace missing data with zero
#summarise command
sumarise_urbanpopulation
sumarise_urbanpopulation <- summarize(group_by(data_2, Urbanization_population), m = mean(Urbanization_population), sd = sd(Urbanization_population))
summarize_urbanization_growth <-summarize(group_by(data_2, population_growth), m = mean(population_growth), sd = sd(population_growth))
#sd
sd1<-sd(data_1)
print(sd1)
#merging datasets
data_3 <- rename(data_2, Year = year)
merging_data <-full_join(data_1, data_3, by = "Year")
merging_data<- na.omit(merging_data)
#'4'Make a graph
library(tidyquant)
library(ggplot2)
#sd
standard_deviation1 <- sd(merging_data$Deaths)
print(standard_deviation1)
standard_deviation2 <- sd(merging_data$Urban)
print(standard_deviation2)
standard_deviation3 <- sd(merging_data$Mag)
print(standard_deviation3)
standard_deviation4 <- sd(merging_data$Focal.Depth..km.)
print(standard_deviation4)
standard_deviation5 <- sd(merging_data$agland)
#sum
sum1<-sum(merging_data$Deaths)
print(sum1)
#'5'Empirical analysis
merging_data <- mutate(merging_data, lnfatalities = log(Deaths))
merging_data <- mutate(merging_data, lnurbanpercentage = (Urban))
merging_data <- mutate(merging_data, lnurbanpopulation= log(urbanpopulation))
merging_data <- mutate(merging_data, lnmagnitude= log(Mag))
merging_data <- mutate(merging_data, lnagriland= (agland))
merging_data <- mutate(merging_data, lndepth = log(Focal.Depth..km.))
#1
data_lm1 <- lm(lnfatalities ~ lnurbanpercentage , data = merging_data )
summary(data_lm1)
#2
data_lm2 <- lm(lnfatalities ~ lnurbanpercentage + lnmagnitude, data = merging_data)
summary(data_lm2)
#3
data_lm3 <- lm(lnfatalities ~ lnurbanpercentage + lnmagnitude + lndepth, data = merging_data)
summary(data_lm3)
#4
data_lm4 <- lm(lnfatalities ~ lnurbanpercentage + lnmagnitude + lndepth + lnagriland , data = merging_data)
summary(data_lm4)

data_lm3 <- lm(lnfatalities ~ lnurbanpercentage + lnmagnitude + lndepth + lnagriland, data = merging_data )
summary(data_lm3)
#draw graph
install.packages("ggplot2")
library(ggplot2)
coefficients<-coef(data_lm1)
chart<-ggplot(merging_data, aes(x = Urban, y = lnfatalities)) + 
  geom_point (color = "blue", size = 3) +
  geom_abline(intercept = coefficients[1], slope = coefficients [2], color = "red")
  labs (title = "Earthquake Fatalities and Urban population", 
        x = "Urban population (%)", 
       y = "Earthquake fatalities")
print(chart)
