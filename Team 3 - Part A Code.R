#-------------------Libraries----------------------------------------------------
library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library("Hmisc")
library(corrplot)
library(tseries)
#install.packages("strucchange", repos = "http://cran.us.r-project.org")
library(strucchange)
#install.packages("Rlof", repos = "http://cran.us.r-project.org")
library(Rlof)
#-------------------Summary & SD & Skewness----------------------------------------------------

dataset<-read.csv(file.choose(),header = T)
summary(dataset)
skewness(dataset$Overall_Score)
sd(dataset$Overall_Score)
skewness(dataset$Property_Rights)
sd(dataset$Property_Rights)
skewness(dataset$Judicial_Effectiveness)
sd(dataset$Judicial_Effectiveness)
skewness(dataset$Government_Integrity)
sd(dataset$Government_Integrity)
skewness(dataset$Tax_Burden)
sd(dataset$Tax_Burden)
skewness(dataset$Government_Spending)
sd(dataset$Government_Spending)
skewness(dataset$Fiscal_Health)
sd(dataset$Fiscal_Health)
skewness(dataset$Business_Freedom)
sd(dataset$Business_Freedom)
skewness(dataset$Labor_Freedom)
sd(dataset$Labor_Freedom)
skewness(dataset$Monetary_Freedom)
sd(dataset$Monetary_Freedom)
skewness(dataset$Trade_Freedom)
sd(dataset$Trade_Freedom)


dataset$Investment_Freedom[dataset$Investment_Freedom == 0 | dataset$Investment_Freedom == 5 | dataset$Investment_Freedom == 10
                           | dataset$Investment_Freedom == 15   | dataset$Investment_Freedom == 20       | dataset$Investment_Freedom == 25] = "Low"
dataset$Investment_Freedom[dataset$Investment_Freedom == 30 | dataset$Investment_Freedom == 35
                           | dataset$Investment_Freedom == 40   | dataset$Investment_Freedom == 45       | dataset$Investment_Freedom == 50] = "Medium"
dataset$Investment_Freedom[dataset$Investment_Freedom == 55 | dataset$Investment_Freedom == 60
                           | dataset$Investment_Freedom == 65   | dataset$Investment_Freedom == 70       | dataset$Investment_Freedom == 75]= "Hight"
dataset$Investment_Freedom[dataset$Investment_Freedom == 80 | dataset$Investment_Freedom == 85
                           | dataset$Investment_Freedom == 90   | dataset$Investment_Freedom == 95       | dataset$Investment_Freedom == 100]= "Very Hight"

Investment_Freedom_table<-prop.table(table(dataset$Investment_Freedom))
barplot(Investment_Freedom_table, main = 'Categories')
dataset1<-subset(dataset,dataset$Investment_Freedom=='Hight')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)
dataset1<-subset(dataset,dataset$Investment_Freedom=='Low')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)
dataset1<-subset(dataset,dataset$Investment_Freedom=='Medium')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)
dataset1<-subset(dataset,dataset$Investment_Freedom=='Very Hight')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)

dataset$Financial_Freedom[dataset$Financial_Freedom == 0  | dataset$Financial_Freedom == 10 | dataset$Financial_Freedom == 20] = "Very Low"
dataset$Financial_Freedom[dataset$Financial_Freedom == 30  | dataset$Financial_Freedom == 40] = "Low"
dataset$Financial_Freedom[dataset$Financial_Freedom == 50  | dataset$Financial_Freedom == 60] = "Medium"
dataset$Financial_Freedom[dataset$Financial_Freedom == 70  | dataset$Financial_Freedom == 80] = "Hight"
dataset$Financial_Freedom[dataset$Financial_Freedom == 90  | dataset$Financial_Freedom == 100] = "Very Hight"

Financial_Freedom_table<-prop.table(table(dataset$Financial_Freedom))
barplot(Financial_Freedom_table, main = 'Categories')
dataset1<-subset(dataset,dataset$Financial_Freedom=='Hight')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)
dataset1<-subset(dataset,dataset$Financial_Freedom=='Low')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)
dataset1<-subset(dataset,dataset$Financial_Freedom=='Medium')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)
dataset1<-subset(dataset,dataset$Financial_Freedom=='Very Hight')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)
dataset1<-subset(dataset,dataset$Financial_Freedom=='Very Low')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)


#-------------------Histograma & Cumulative Distribution----------------------------------------------------

hist (dataset$Overall_Score, prob=TRUE, main='Overall Score', xlab='Overall Score', col="grey")
lines(density(dataset$Overall_Score),col="red", lwd=2)
x <- ecdf(dataset$'Overall_Score')
plot(x,main='cumulative distribution', xlab='Overall Score')

hist (dataset$Government_Integrity, prob=TRUE, main='Government_Integrity', xlab='Government_Integrity', col="grey")
lines(density(dataset$Government_Integrity),col="red", lwd=2)
x <- ecdf(dataset$'Government_Integrity')
plot(x,main='cumulative distribution', xlab='Government_Integrity')

hist (dataset$Tax_Burden, prob=TRUE, main='Tax_Burden', xlab='Tax_Burden', col="grey")
lines(density(dataset$Tax_Burden),col="red", lwd=2)
x <- ecdf(dataset$'Tax_Burden')
plot(x,main='cumulative distribution', xlab='Tax_Burden')

hist (dataset$Fiscal_Health, prob=TRUE, main='Fiscal_Health', xlab='Fiscal_Health', col="grey")
lines(density(dataset$Fiscal_Health),col="red", lwd=2)
x <- ecdf(dataset$'Fiscal_Health')
plot(x,main='cumulative distribution', xlab='Fiscal_Health')


#-------------------Variables----------------------------------------------------

y  <- dataset$Overall_Score
x1 <- dataset$Property_Rights
x2 <- dataset$Judicial_Effectiveness
x3 <- dataset$Government_Integrity
x4 <- dataset$Tax_Burden
x5 <- dataset$Government_Spending
x6 <- dataset$Fiscal_Health
x7 <- dataset$Business_Freedom
x8 <- dataset$Labor_Freedom
x9 <- dataset$Monetary_Freedom
x10 <- dataset$Trade_Freedom
x11 <- dataset$Investment_Freedom
x12 <- dataset$Financial_Freedom



