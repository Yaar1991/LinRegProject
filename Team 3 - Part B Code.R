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
library(car)
library(mixlm)

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

stripplot(dataset$Overall_Score,xlab='Overall_Score' )

stripplot(dataset$Property_Rights , offset=1/3)
stripplot(dataset$Property_Rights )
stripplot(dataset$Fiscal_Health)
stripplot(dataset$Trade_Freedom)
stripplot(dataset$Monetary_Freedom)
stripplot(dataset$Government_Integrity)
stripplot(dataset$Property_Rights)

dataset$Investment_Freedom[dataset$Investment_Freedom == 0 | dataset$Investment_Freedom == 5 | dataset$Investment_Freedom == 10
                           | dataset$Investment_Freedom == 15   | dataset$Investment_Freedom == 20] = "1"
dataset$Investment_Freedom[dataset$Investment_Freedom == 25 | dataset$Investment_Freedom == 30 | dataset$Investment_Freedom == 35
                           | dataset$Investment_Freedom == 40   | dataset$Investment_Freedom == 45       | dataset$Investment_Freedom == 50 | dataset$Investment_Freedom == 55] = "2"
dataset$Investment_Freedom[ dataset$Investment_Freedom == 60   | dataset$Investment_Freedom == 65   | dataset$Investment_Freedom == 70       | dataset$Investment_Freedom == 75]= "3"
dataset$Investment_Freedom[dataset$Investment_Freedom == 80 | dataset$Investment_Freedom == 85
                           | dataset$Investment_Freedom == 90   | dataset$Investment_Freedom == 95       | dataset$Investment_Freedom == 100]= "4"

Investment_Freedom_table<-prop.table(table(dataset$Investment_Freedom))
barplot(Investment_Freedom_table, main = 'Categories')
dataset1<-subset(dataset,dataset$Investment_Freedom=='1')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)
dataset1<-subset(dataset,dataset$Investment_Freedom=='2')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)
dataset1<-subset(dataset,dataset$Investment_Freedom=='3')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)
dataset1<-subset(dataset,dataset$Investment_Freedom=='4')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)

dataset$Financial_Freedom[dataset$Financial_Freedom == 0  | dataset$Financial_Freedom == 10 | dataset$Financial_Freedom == 20] = "1"
dataset$Financial_Freedom[dataset$Financial_Freedom == 30  | dataset$Financial_Freedom == 40 | dataset$Financial_Freedom == 50] = "2"
dataset$Financial_Freedom[dataset$Financial_Freedom == 60 | dataset$Financial_Freedom == 70] = "3"
dataset$Financial_Freedom[dataset$Financial_Freedom == 80 | dataset$Financial_Freedom == 90  | dataset$Financial_Freedom == 100] = "4"

Financial_Freedom_table<-prop.table(table(dataset$Financial_Freedom))
barplot(Financial_Freedom_table, main = 'Categories')
dataset1<-subset(dataset,dataset$Financial_Freedom=='1')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)
dataset1<-subset(dataset,dataset$Financial_Freedom=='2')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)
dataset1<-subset(dataset,dataset$Financial_Freedom=='3')
summary(dataset1$Overall_Score)
sd(dataset1$Overall_Score)
skewness(dataset1$Overall_Score)
dataset1<-subset(dataset,dataset$Financial_Freedom=='4')
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

dataset$Investment_Freedom[dataset$Investment_Freedom == 0 | dataset$Investment_Freedom == 5 | dataset$Investment_Freedom == 10
                           | dataset$Investment_Freedom == 15   | dataset$Investment_Freedom == 20] = "1"
dataset$Investment_Freedom[dataset$Investment_Freedom == 25 | dataset$Investment_Freedom == 30 | dataset$Investment_Freedom == 35
                           | dataset$Investment_Freedom == 40   | dataset$Investment_Freedom == 45       | dataset$Investment_Freedom == 50 | dataset$Investment_Freedom == 55] = "2"
dataset$Investment_Freedom[ dataset$Investment_Freedom == 60   | dataset$Investment_Freedom == 65   | dataset$Investment_Freedom == 70       | dataset$Investment_Freedom == 75]= "3"
dataset$Investment_Freedom[dataset$Investment_Freedom == 80 | dataset$Investment_Freedom == 85
                           | dataset$Investment_Freedom == 90   | dataset$Investment_Freedom == 95       | dataset$Investment_Freedom == 100]= "4"

dataset$Financial_Freedom[dataset$Financial_Freedom == 0  | dataset$Financial_Freedom == 10 | dataset$Financial_Freedom == 20] = "1"
dataset$Financial_Freedom[dataset$Financial_Freedom == 30  | dataset$Financial_Freedom == 40 | dataset$Financial_Freedom == 50] = "2"
dataset$Financial_Freedom[dataset$Financial_Freedom == 60 | dataset$Financial_Freedom == 70] = "3"
dataset$Financial_Freedom[dataset$Financial_Freedom == 80 | dataset$Financial_Freedom == 90  | dataset$Financial_Freedom == 100] = "4"

group <- c(dataset$Continent)
dataset$Continent<-gsub("Asia", "1", group)

dataset$Continent= as.numeric(dataset$Continent)

dataset$Continent[dataset$Continent == 2 ] = 12
dataset$Continent[dataset$Continent == 4 ] = 14
dataset$Continent[dataset$Continent == 1 ] = 11
dataset$Continent[dataset$Continent == 6 ] = 16
dataset$Continent[dataset$Continent == 8 ] = 18
dataset$Continent[dataset$Continent == 3 ] = 13
dataset$Continent[dataset$Continent == 5 ] = 15
dataset$Continent[dataset$Continent == 7 ] = 17
dataset$Continent[dataset$Continent == 11] = 0
dataset$Continent[dataset$Continent == 12] = 1
dataset$Continent[dataset$Continent == 15] = 2
dataset$Continent[dataset$Continent == 13] = 3
dataset$Continent[dataset$Continent == 18] = 3
dataset$Continent[dataset$Continent == 17] = 4
dataset$Continent[dataset$Continent == 14] = 5
dataset$Continent[dataset$Continent == 16] = 5

dataset$Investment_Freedom= as.numeric(dataset$Investment_Freedom)
dataset$Financial_Freedom=as.numeric(dataset$Financial_Freedom)

y  <- dataset$Overall_Score
x1 <- dataset$Property_Rights
x2 <- dataset$Judicial_Effectiveness
x3 <- dataset$Government_Integrity
x4 <- dataset$Tax_Burden
x6 <- dataset$Fiscal_Health
x7 <- dataset$Business_Freedom
x8 <- dataset$Labor_Freedom
x9 <- dataset$Monetary_Freedom
x10 <- dataset$Trade_Freedom
x11 <- dataset$Investment_Freedom
x12 <- dataset$Financial_Freedom
x13 <- dataset$Legislation_Violence
x14 <- dataset$Continent


#-----------------Remove Variables----------------------
fit<-lm(y ~ x1)
summary(fit)
fit<-lm(y ~ x2)
summary(fit)
fit<-lm(y ~ x3)
summary(fit)
fit<-lm(y ~ x4)
summary(fit)
fit<-lm(y ~ x5)
summary(fit)
fit<-lm(y ~ x6)
summary(fit)
fit<-lm(y ~ x7)
summary(fit)
fit<-lm(y ~ x8)
summary(fit)
fit<-lm(y ~ x9)
summary(fit)
fit<-lm(y ~ x10)
summary(fit)


fit<-lm(y ~ x1+x2+x3+x4+x6+x7+x8+x9+x10)
summary(fit)

dataset = subset(dataset, select = -c(Name,Continent,Investment_Freedom,Financial_Freedom,Legislation_Violence))
cor(dataset, use = "complete.obs",method = "pearson")

col<-cor(dataset, method=c("pearson"))

plot(y=y, x=x1, ylab='Overall Score', xlab='Property_Rights')
plot(y=y, x=x2, ylab='Overall Score', xlab='Government_Integrity')
plot(y=y, x=x3, ylab='Overall Score', xlab='Judicial_Effectiveness')
plot(y=y, x=x5, ylab='Overall Score', xlab='Government_Spending')


dataset$Investment_Freedom<-as.integer(dataset$Investment_Freedom)
plot(y=y, x=dataset$Investment_Freedom, ylab='Overall Score', xlab='Investment_Freedom')

dataset$Financial_Freedom<-as.integer(dataset$Financial_Freedom)
plot(y=y, x=dataset$Financial_Freedom, ylab='Overall Score', xlab='Financial_Freedom')

plot(y=y, x=x13, ylab='Overall Score', xlab='Legislation_Violence')
x14<-as.integer(x14)
plot(y=y, x=x14, ylab='Overall Score', xlab='Continent')

dataset$Investment_Freedom = factor(dataset$Investment_Freedom)

dataset$Investment_Freedom<-as.integer(dataset$Investment_Freedom)
plot(y=y, x=dataset$Investment_Freedom, ylab='Overall Score', xlab='Investment Freedom', pch=19)
dataset$Financial_Freedom<-as.integer(dataset$Financial_Freedom)
plot(y=y, x=dataset$Financial_Freedom, ylab='Overall Score', xlab='Financial Freedom', pch=19)

#---------------------------------------------------Model Results Check--------------------------------------------------------------------------------------D
ggplot(data=datab,aes(x=Overall_Score,y=Judicial_Effectiveness,color=(Financial_Freedom)))+geom_jitter(aes(x=Overall_Score,y=Judicial_Effectiveness,color=(Financial_Freedom)))+geom_smooth(method = "lm" ,se = FALSE)+labs(y= " Judicial Effectiveness ", x = "Overall Score") + labs(color="Financial Freedom")

ggplot(data=datab,aes(x=Overall_Score,y=Government_Integrity,color=(Financial_Freedom)))+geom_jitter(aes(x=Overall_Score,y=Government_Integrity,color=(Financial_Freedom)))+geom_smooth(method = "lm" ,se = FALSE)+labs(y= "Government Integrity", x = "Overall Score") + labs(color="Financial Freedom")

ggplot(data=datab,aes(x=Overall_Score,y=Business_Freedom,color=(Invesment_Freedom)))+geom_jitter(aes(x=Overall_Score,y= Business_Freedom, color=(Financial_Freedom)))+geom_smooth(method = "lm" ,se = FALSE)+labs(y= "Business Freedom", x = "Overall_Score") + labs(color="Invesment Freedom")

ggplot(data=datab,aes(x=Overall_Score,y= Judicial_Effectiveness,color=(Investment_Freedom)))+geom_jitter(aes(x=Overall_Score,y= Judicial_Effectiveness, color=(Investment_Freedom)))+geom_smooth(method = "lm" ,se = FALSE) + labs(y= "Business_Freedom", x = "Overall Score") + labs(color="Investment Freedom")

ggplot(data=datab,aes(x=Overall_Score,y= Monetary_Freedom,color=(Continent)))+geom_jitter(aes(x=Overall_Score,y= Monetary_Freedom, color=(Continent)))+geom_smooth(method = "lm" ,se = FALSE)+ labs(y= "Monetary Freedom", x = "Overall_Score") + labs(color="Continent")

#---------------------------------------------------Choosing Variables--------------------------------------------------------

TestModel<-lm(formula = y~x1+x2+x3+x4+x6+x7+x8+x9+x10+factor(x13)+factor(dataset$Continent)+factor(x12)+factor(x11)+factor(x12):x3+factor(dataset$Continent):x9, data = dataset)

anova(TestModel)
summary(TestModel)
AIC<-AIC(TestModel)
BIC<-AIC(TestModel, k=log(179))
print(AIC)
print(BIC)

#---------------------------------------------------Back--------------------------------------------------------

#backward regression- start with full data

#AIC
BackAIC<-step(TestModel,direction = "backward",test="F")
AIC<-AIC(BackAIC)
anova(BackAIC)
summary(BackAIC)
BIC<-AIC(BackAIC, k=log(179))
print(AIC)
print(BIC)

#BIC
BackBIC<-step(TestModel, direction = "backward",k=log(179))
AIC<-AIC(BackBIC)
BIC<-AIC(BackBIC, k=log(179))
summary(BackBIC)
print(AIC)
print(BIC)

#---------------------------------------------------Forward--------------------------------------------------------

#forward regression- start with empty data
forwardRegression<-lm(y~1, data=dataset)
#AIC
forwardAIC<-step(forwardRegression, direction = "forward",test="F",scope = formula(TestModel))
AIC<-AIC(forwardAIC)
BIC<-AIC(forwardAIC, k=log(179))
summary(forwardAIC)
print(AIC)
print(BIC)

#BIC
forwardBIC<-step(forwardRegression, direction = "forward",scope = formula(TestModel),k=log(179))
AIC<-AIC(forwardBIC)
BIC<-AIC(forwardBIC, k=log(179))
summary(forwardBIC)
print(AIC)
print(BIC)

#---------------------------------------------------steps------------------------------------------------------

#stepwise regression
#AIC
StepwiseAIC<-step(forwardRegression,direction = "both",scope = formula(TestModel))
AIC<-AIC(StepwiseAIC)
BIC<-AIC(StepwiseAIC, k=log(179))
summary(StepwiseAIC)
print(AIC)
print(BIC)

#BIC
StepwiseBIC<-step(forwardRegression,direction = "both",scope=formula(TestModel), k=log(179))
AIC<-AIC(StepwiseBIC)
BIC<-AIC(StepwiseBIC, k=log(179))
summary(StepwiseBIC)
print(AIC)
print(BIC)



#---------------------------------------------------Choosing the Model--------------------------------------------------------

FinalModel<-lm(formula = y ~ x1 + x2 + x3 + x4 + x6 + x7 + x8 + x9 + x10 + 
               factor(dataset$Continent) + factor(x12) + factor(x11) + x3:factor(x12) + 
               x9:factor(dataset$Continent))
anova(FinalModel)
print(FinalModel)
summary(FinalModel)
AIC<-AIC(FinalModel)
BIC<-AIC(FinalModel, k=log(179))
print(AIC)
print(BIC)

#--------------------------------------------------Model Assumptions------------------------------------------------------------------


FinalModel.res <- resid(FinalModel)
FinalModel.res
summary(FinalModel)

#---equality of variances tset 

#Residuals vs. Fitted Plot
Predicted <- fitted(FinalModel)
unstdResiduals <- residuals(FinalModel)
stdResiduals <- (unstdResiduals) / sd(unstdResiduals)
plot(Predicted, stdResiduals, main = "Residuals vs. Fitted Plot", xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(0,0)

#test
install.packages("lmtest")
library(lmtest)
gqtest(FinalModel,alternative="two.sided")

#---normal check 
#qqplot
qqnorm(FinalModel.res,datax = FALSE)
qqline(FinalModel.res,datax = FALSE)

#hist
hist(stdResiduals, prob=TRUE, main='Standardized residuals',ylim=c(0,0.55), xlab='Standardized residuals' ) 
lines(density(stdResiduals), col="black", lwd=2) 

#ks test 
ks.test(x= stdResiduals,y="pnorm",alternative ="two.sided", exact = NULL)


#------------------------------------Model Improvment-----------------------------------------------------------
#---------------------ln(x)---------------------------
lnx <- dataset[,c(1,2,3,5,6,7,8,9,10)]
lnx$Property_Rights <- log(lnx$Property_Rights)
lnx$Judicial_Effectiveness <- log(lnx$Judicial_Effectiveness)
lnx$Tax_Burden <- log(lnx$Tax_Burden)
lnx$Fiscal_Health <- log(lnx$Fiscal_Health)
lnx$Business_Freedom <- log(lnx$Business_Freedom)
lnx$Labor_Freedom <- log(lnx$Labor_Freedom)
lnx$Monetary_Freedom <- log(lnx$Monetary_Freedom)
lnx$Trade_Freedom <- log(lnx$Trade_Freedom)
corrlnx<-cor(lnx, use="complete.obs", method="pearson")
print(corrlnx)


#----------------sqrt(x)-----------------
print(dataset)
sqrtx <- dataset[,c(1,2,3,5,6,7,8,9,10)]
sqrtx$Prprint()y_Rights <- sqrt(sqrtx$Property_Rights)
sqrtx$Judicial_Effectiveness <- sqrt(sqrtx$Judicial_Effectiveness)
sqrtx$Tax_Burden <- sqrt(sqrtx$Tax_Burden)
sqrtx$Fiscal_Health <- sqrt(sqrtx$Fiscal_Health)
sqrtx$Business_Freedom <- sqrt(sqrtx$Business_Freedom)
sqrtx$Labor_Freedom <- sqrt(sqrtx$Labor_Freedom)
sqrtx$Monetary_Freedom <- sqrt(sqrtx$Monetary_Freedom)
sqrtx$Trade_Freedom <- sqrt(sqrtx$Trade_Freedom)

corrsqrtx<-cor(sqrtx, use="complete.obs", method="pearson")
print(corrsqrtx)


#---------------(X)^2---------------
powerx <- dataset[,c(1,2,3,5,6,7,8,9,10)]
powerx$Property_Rights <- (powerx$Property_Rights)^2
powerx$Judicial_Effectiveness <- (powerx$Judicial_Effectiveness)^2
powerx$Tax_Burden <- (powerx$Tax_Burden)^2
powerx$Fiscal_Health <- (powerx$Fiscal_Health)^2
powerx$Business_Freedom <- (powerx$Business_Freedom)^2
powerx$Labor_Freedom <- (powerx$Labor_Freedom)^2
powerx$Monetary_Freedom <- (powerx$Monetary_Freedom)^2
powerx$Trade_Freedom <- (powerx$Trade_Freedom)^2


corrpowerx<-cor(powerx, use="complete.obs", method="pearson")
print(corrpowerx)


NewFM<-lm(formula = y ~ x1^2 + sqrt(x2) + x4 + x6 + x7 + x8 + x9 + x10 + factor(dataset$Continent) + 
            factor(dataset$Financial_Freedom) + factor(dataset$Investment_Freedom) + 
            x3:factor(dataset$Financial_Freedom) + x9:factor(dataset$Continent), 
          data = dataset)


#-----------------------------Forward ---------------------------------------
#-------------AIC-------------
NewforwardAIC <- step(lm(y~1, data=dataset), direction = "forward",scope = ~x1^2 + sqrt(x2) + x4 + x6 + x7 + x8 + x9 + x10 + factor(dataset$Continent) + 
                        factor(dataset$Financial_Freedom) + factor(dataset$Investment_Freedom) + 
                        x3:factor(dataset$Financial_Freedom) + x9:factor(dataset$Continent))
summary(NewforwardAIC)
AIC(NewforwardAIC)
BIC(NewforwardAIC)
#-------------BIC-------------
NewforwardBIC = step(lm(y~1, data=dataset), direction = "forward",scope = ~  x1^2 + sqrt(x2) + x4 + x6 + x7 + x8 + x9 + x10 + factor(dataset$Continent) + 
                       factor(dataset$Financial_Freedom) + factor(dataset$Investment_Freedom) + 
                       x3:factor(dataset$Financial_Freedom) + x9:factor(dataset$Continent) ,  k=log(179))
summary(NewforwardBIC)
BIC(NewforwardBIC)
AIC(NewforwardBIC)

#-----------------------------Backward---------------------------------------
#-------------AIC-------------
NewBackAIC = step(NewFM , direction = "backward")
summary(NewBackAIC)
AIC(NewBackAIC)
BIC(NewBackAIC)
#-------------BIC-------------
NewBackBIC = step(NewFM, direction = "backward", k=log(179))
summary(NewBackBIC)
BIC(NewBackBIC)
AIC(NewBackBIC)

#-----------------------------stepwise---------------------------------------
#-------------AIC-------------
NewstepwiseAIC = step(lm(y~1, data=dataset), direction = "both",scope = ~x1^2 + sqrt(x2) + x4 + x6 + x7 + x8 + x9 + x10 + factor(dataset$Continent) + 
                        factor(dataset$Financial_Freedom) + factor(dataset$Investment_Freedom) + 
                        x3:factor(dataset$Financial_Freedom) + x9:factor(dataset$Continent))
summary(NewstepwiseAIC)
AIC(NewstepwiseAIC)
BIC(NewstepwiseAIC)

#-------------BIC-------------
NewstepwiseBIC = step(lm(y~1, data=dataset), direction = "both",scope = ~x1^2 + sqrt(x2) + x4 + x6 + x7 + x8 + x9 + x10 + factor(dataset$Continent) + 
                        factor(dataset$Financial_Freedom) + factor(dataset$Investment_Freedom) + 
                        x3:factor(dataset$Financial_Freedom) + x9:factor(dataset$Continent),  k=log(179))
summary(NewstepwiseBIC)
AIC(NewstepwiseBIC)
BIC(NewstepwiseBIC)
