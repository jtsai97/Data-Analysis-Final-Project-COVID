library(psych)
library(plyr)
library(ggplot2)
library(gmodels)

data <- read.csv('final-project.csv')

summary(data$containment_index)
summary(data$stringency_index)
summary(data$new_vaccinations_smoothed_per_million)
summary(data$new_cases)
summary(data$new_deaths)


#categorize with 0 being no support and 1 being support 
table(data$debt_relief)
data$debt = ifelse((data$debt_relief>0),1,0)
table(data$debt)

#categorize with 0 being no support and 1 being support given
table(data$income_support)
data$income = ifelse((data$income_support>0),1,0)
table(data$income)

lm1 = lm(debt_relief + income_support ~ containment_index + 
           stringency_index + new_vaccinations_smoothed_per_million + 
           new_cases + new_deaths,data)
summary(lm1)

lm2 = lm(debt_relief ~ containment_index + stringency_index + new_vaccinations_smoothed_per_million + new_cases + new_deaths,data) 
summary(lm2)

lm3 = lm(income_support ~ containment_index + stringency_index + new_vaccinations_smoothed_per_million + new_cases + new_deaths,data)
summary(lm3)

lm4 = lm(debt + income ~ containment_index + stringency_index + new_vaccinations_smoothed_per_million + new_cases + new_deaths,data)
summary(lm4)

lm5 = lm(debt ~ containment_index + stringency_index + new_vaccinations_smoothed_per_million + new_cases + new_deaths,data) 
summary(lm5)

lm6 = lm(income ~ containment_index + stringency_index + new_vaccinations_smoothed_per_million + new_cases + new_deaths,data) 
summary(lm6)

logit1 = glm(debt ~ containment_index + stringency_index + new_vaccinations_smoothed_per_million + new_cases + new_deaths,binomial,data)
summary(logit1)

logit2 = glm(income ~ containment_index + stringency_index + new_vaccinations_smoothed_per_million + new_cases + new_deaths,binomial,data)
summary(logit2) 


exp(coef(logit2))

logit3 = glm(income ~ containment_index + stringency_index + new_vaccinations_smoothed_per_million*new_deaths + new_cases,binomial,data)
summary(logit3) 
exp(coef(logit3))

anova(logit2,logit3,test='Chisq')
