library(ggplot2)
library(dplyr)
library(readxl)
library(ggpubr)

data<-read.csv("loan_level_500k.csv")
setwd("~/GitHub/data-for-good")
data <- na.omit(data)
m <- lm(formula = ORIGINAL_INTEREST_RATE~ CREDIT_SCORE+ 
     ORIGINAL_DEBT_TO_INCOME_RATIO+NUMBER_OF_BORROWERS,data=data)
anova(m)
summary(m)


m1 <- lm(formula = ORIGINAL_INTEREST_RATE~ CREDIT_SCORE+ 
          ORIGINAL_DEBT_TO_INCOME_RATIO+NUMBER_OF_BORROWERS+
           POSTAL_CODE,data=data)

anova(m1)
summary(m1)

