rm(list=ls(all=TRUE))

# Obligatory exercise 1

# Reading datafiles
no2data <- read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v20/mandatory/no2.txt",sep="\t",header=TRUE)

# 1a)
# Catogorizing the data
names(no2data)=c("log.no2","log.cars","temp","wind.speed","hour.of.day")

# Summary of the pollution levels and number of cars
summary(no2data)
summary(no2data$log.cars)

# Boxplot of log.no2 and log.cars
boxplot(no2data$log.no2, no2data$log.cars, names = c("log.no2", "log.cars"), main = "Boxplot for the cars and NO2")

# Scatterplot of log.cars and log.no2
plot(no2data$log.cars, no2data$log.no2, main = "Scatterplot of log.cars and log.no2")

# 1b)
# Linear fit
fit=lm(no2data$log.no2~no2data$log.cars)
summary(fit)

# Scatterplot with the linearfit
plot(no2data$log.cars, no2data$log.no2, main = "Scatterplot with the linearfit")
abline(fit, col = "blue")


# Correlation between no2data$log.cars,no2data$log.no2
cor(no2data$log.cars,no2data$log.no2)**2

# 1c)
# Linearity check
library(car)
crPlots(fit, terms=~no2data$log.cars)

# Checking for constant variance
par(mfrow = c(2,2))
plot(fit)

# Checking for normality
par(mfrow = c(1,2))
hist(fit$res)
boxplot(fit$res, main = "Boxplot for the fitted model")
qqnorm(fit$res); qqline(fit$res)

# 1d)

# Correlation check
cor(no2data$log.cars,no2data$hour.of.day)


# Fitting the model of the multiple linear regression
Multireg = lm(no2data$log.no2~no2data$log.cars+no2data$temp+no2data$wind.speed + no2data$hour.of.day)
summary(Multireg)


boxplot(no2data$temp, no2data$wind.speed, no2data$hour.of.day , names = c("temp", "wind.speed", "hour"), main = "Boxplot temp, wind speed, hour")

newwind.speed=log(no2data$wind.speed)


# Fitting of the multiple linear regression model without hours.per.day as it is closely correlated to log.cars 
fit.multiregfinal = lm(no2data$log.no2~no2data$log.cars+newwind.speed+no2data$temp)
summary(fit.multiregfinal)

cor(newwind.speed,no2data$hour.of.day)
cor(no2data$log.no2,no2data$hour.of.day)
cor(no2data$temp,no2data$hour.of.day)

#1e)Summary of the final model

crPlots(fit.multiregfinal, terms=~no2data$log.cars+no2data$temp+newwind.speed)

par(mfrow = c(2, 2))
plot(fit.multiregfinal)

# 2a)

# Reading in data
blood <-"http://www.uio.no/studier/emner/matnat/math/STK4900/v20/mandatory/blood.txt"
blood <-read.table(blood, header = TRUE)
blood$age <- factor(blood$age)

blood %>% 
group_by(age) %>% 
summarize(q1 = quantile(Bloodpr, 0.25), q3 = quantile(Bloodpr, 0.75), Mean = mean(Bloodpr), Sd = sd(Bloodpr), min= min(Bloodpr), max=max(Bloodpr))

Bloodpr=blood$Bloodpr
boxplot(blood$Bloodpr ~ blood$age, xlab = "Age Group", ylab = "Blood Pressure", main = "Blood pressure and the age groups")

#2b) One way anova and summary
aov.blood = aov(Bloodpr~blood$age, data = blood)
summary(aov.blood)

#2c) Formulate this problem using a regression model with age group as categorical 
#predictor variable. Use treatment-contrast and the youngest group as reference. 
#Run the analysis, interpret the results and write a conclusion. 
#Compare with the analysis in b).

# Doing the linear regression and summary of the fit by the traditional way (without a loop)
fit.blood = lm(Bloodpr~blood$age, data = blood)
summary(fit.blood)
# There is another way to make it possible via loop. The cole is here:
#https://stats.idre.ucla.edu/r/modules/coding-for-categorical-variables-in-regression-models/?fbclid=IwAR1q2l3fR8CHB7DUDhST6wyaOHemFpvckUzZXfYOZg5qvAmj49WddRBxFQ4

blood <- within(blood, {
Agegroup <- C(age, treatment)
print(attributes(Agegroup))
})
summary(lm(Bloodpr ~ Agegroup, data=blood))

saveRDS(object,file = "My_data.rds")