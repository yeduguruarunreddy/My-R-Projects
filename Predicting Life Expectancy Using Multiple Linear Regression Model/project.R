#install.packages("dplyr")
#install.packages("car")
#install.packages("leaps")


options(scipen=999)

library(dplyr)
library(car)
library(leaps)


setwd('C:\\Users\\arunr\\Downloads')

data = read.csv("Life Expectancy Data.csv",header=T)

dim(data)

sum(is.na(data))

data = na.omit(data)

dim(data)

data_numeric = select_if(data, is.numeric)

dim(data_numeric)

cor_data = cor(data_numeric)
cor_data

corrplot(data_numeric)

cor_data.style.background_gradient(cmap='coolwarm')
cor_data.savefig('result.png', bbox_inches='tight', pad_inches=0.0)

print("Correlation matrix")
print(cor_data)

pairs(data_numeric)


plot(data$Adult.Mortality, data$Life.expectancy, pch = 19, col = "black")
plot(data$Income.composition.of.resources, data$Life.expectancy, pch = 19, col = "black")
plot(data$Schooling, data$Life.expectancy, pch = 19, col = "black")

# plot(data$Status, data$Life.expectancy, pch = 19, col = "black")

model1 <- lm(Life.expectancy ~ . , data=data_numeric)
summary(model1)

vif(model1)

plot(data$infant.deaths, data$under.five.deaths, pch = 19, col = "black")
plot(data$percentage.expenditure, data$GDP, pch = 19, col = "black")

cor(data$Life.expectancy, data$infant.deaths)
cor(data$Life.expectancy, data$under.five.deaths)

cor(data$Life.expectancy, data$percentage.expenditure)
cor(data$Life.expectancy, data$GDP)

data <- subset(data, select = -c(infant.deaths,percentage.expenditure))
dim(data)

model2 <- lm(Life.expectancy ~ . , data=data)
summary(model2)






regfit.fwd = regsubsets(Life.expectancy ~ . , data=data, nvmax=150, method="forward")

reg.fwd = summary(regfit.fwd)


regfit.bwd = regsubsets(Life.expectancy ~ . , data=data, nvmax=150, method="backward")

reg.bwd = summary(regfit.bwd)



par(mfrow=c(2,2))

reg.summary=reg.fwd;

plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="ADJ Rsq",type="l")

which.max(reg.summary$adjr2)
  
points(142,reg.summary$adjr2[142],col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="CP",type="l")

which.min(reg.summary$cp)

points(141, reg.summary$cp[141], col="red",cex=2,pch=20)
which.min(reg.summary$bic)


plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC",type="l")
points(138, reg.summary$bic[138], col="red", cex=2, pch=20)



coef(regfit.fwd,142)


par(mfrow=c(2,2))

reg.summary=reg.bwd;

plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="ADJ Rsq",type="l")

which.max(reg.summary$adjr2)

points(134,reg.summary$adjr2[134],col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="CP",type="l")

which.min(reg.summary$cp)

points(131, reg.summary$cp[131], col="red",cex=2,pch=20)
which.min(reg.summary$bic)

plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC",type="l")
points(126, reg.summary$bic[126], col="red", cex=2, pch=20)



coef(regfit.bwd,134)


model2 <- lm(Life.expectancy ~ Country+ Year+ Adult.Mortality+ Alcohol+ Hepatitis.B+ Measles
             + under.five.deaths+ Polio+ GDP+ Population+ thinness.5.9.years+ Income.composition.of.resources
             + Schooling+ Status, data=data)

summary(model2)






model3 <- lm(Life.expectancy ~ Country+ Year+ Alcohol+ Hepatitis.B+ Measles
             + Polio+ GDP+ Income.composition.of.resources
             + Schooling+ Status, data=data)

summary(model3)





# MODEL ADEQUECY

residuals(model2)

rstandard(model2)

rstudent(model2)

par(mfrow=c(2,2))
plot(model2)

par(mfrow=c(2,2))
plot(model3)


n=nrow(data)
k=14+1
qt(1-0.05/2,n-k)  



