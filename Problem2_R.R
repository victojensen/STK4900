rm(list=ls(all=TRUE))

# a. Reading datafiles
crabs <- read.table("https://www.uio.no/studier/emner/matnat/math/STK4900/data/crabs.txt",header=T) 
summary(crabs) 

# Choose a binary logistic regression model 
fit.width=glm(y~width , data=crabs ,family=binomial) 
summary(fit.width)

# b. Computing the odd ratio 
delta = 1 
OR = exp(fit.width[["coefficients"]][["width"]]*delta) 


# Finding confidence interval 

expcoef=function(glmobj) 
{ 
  regtab=summary(glmobj)$coef 
  expcoef=exp(regtab[,1]) 
  lower=expcoef*exp(-1.96*regtab[,2]) 
  upper=expcoef*exp(1.96*regtab[,2]) 
  cbind(expcoef ,lower ,upper)
  } 
expcoef(fit.width) 

# c. Binary logistic regression model for the other covariates one by one 
fit.width=glm(y~width , data=crabs ,family=binomial)
fit.weight=glm(y~weight , data=crabs ,family=binomial)
fit.color=glm(y~factor(color), data=crabs ,family=binomial) 
fit.spine=glm(y~factor(spine), data=crabs ,family=binomial)
summary(fit.width)
summary(fit.weight)
summary(fit.color) 
summary(fit.spine) 

# Grouped logistic fit for all significant covariates 
fit.multi=glm(y~width+weight+factor(color)+factor(spine), data = crabs , family = binomial) 
summary(fit.multi) 

# d. Grouped logistic fit for all significant covariates
fit.multisig=glm(y~width+weight , data = crabs , family = binomial) 
summary(fit.multisig) 

# Checking correlation between width and weight 
plot(crabs$weight , crabs$width , xlab = "Weight [kg]", ylab = "Width [cm]") 
cor(crabs$weight , crabs$width)

# Fitting the final model 
fit.multisig=glm(y~width , data = crabs , family = binomial) 
summary(fit.multisig) 

# e. Checking interaction 
fit.multiint=glm(y~width+ factor(color) + width:factor(color), data = crabs , family = binomial) 
fit.multiint=glm(y~width+ factor(spine) + width:factor(spine), data = crabs , family = binomial)
fit.multiint=glm(y~factor(color)+ factor(spine) + factor(color):factor(spine), data = crabs , family = binomial) 
summary(fit.multiint)

#Exercise 2. 
# a. Reading in data 
olympic=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/olympic.txt",sep="\t",header=TRUE)

# Making a fit for all covariates
fit.1 = glm(Total2000~offset(Log.athletes) + Total1996 + Log.population + GDP.per.cap ,data=olympic ,family=poisson) 
summary(fit.1) 

# Checking correlation between covariates 
plot(olympic) 
cor(olympic$Total2000 , olympic$Total1996) 

# Making a fit without 1996 
fit.2 = glm(Total2000~offset(Log.athletes)+Log.population + GDP.per.cap,data=olympic ,family=poisson) 
summary(fit.2) 

# Making a fit without GDP and 1996 
fit.3 = glm(Total2000~offset(Log.athletes)+Log.population ,data=olympic ,family=poisson) 
summary(fit.3) 

# Computing rate ratio 
exp(fit.3$coefficients)

#Exercise 3. 
#a. Reading in data 2 
cirrhosis <-  read.table("https://www.uio.no/studier/emner/matnat/math/STK4900/data/cirrhosis.txt",header=TRUE)

library(survival)

survpred <- survfit(Surv(time,status)~1, conf.type="none") 
summary(survpred,xlab="Time(days)",ylab="Survival")

# Plotting the Kaplan-Meier estimate for each fit 
plot(survpred)

survpred1 <- survfit(Surv(time,status)~treat, conf.type="plain")
summary(survpred1)
plot(survpred1,col=c('blue', 'red'),xlab="Time(days)",ylab="Survival") 
legend('topright', c("prednisone","placebo"), col=c('blue', 'red'), lty=1)
title("Treatment")

survpred2 <- survfit(Surv(time,status)~sex, conf.type="plain")
summary(survpred2)
plot(survpred2,col=c('blue', 'red'),xlab="Time(days)",ylab="Survival") 
legend('topright', c("Male","Female"), col=c('blue', 'red'), lty=1)
title("Sex")

survpred3 <- survfit(Surv(time,status)~asc, conf.type="plain")
summary(survpred3)
plot(survpred3,col=c('blue', 'red', 'green'),xlab="Time(days)",ylab="Survival") 
legend('topright', c("none","slight", "marked"), col=c('blue', 'red', 'green'), lty=1)
title("Ascites")

survpred4 <- survfit(Surv(time,status)~agegr, conf.type="plain")
summary(survpred4)
plot(survpred4,col=c('blue', 'red', 'green'),xlab="Time(days)",ylab="Survival") 
legend('topright', c("<50","50-65", ">65"), col=c('blue', 'red', 'green'), lty=1)
title("Age group")

#b) # Logrank tests

survdiff(Surv(time,status)~sex + age)
survdiff(Surv(time,status)~treat)
survdiff(Surv(time,status)~sex)
survdiff(Surv(time,status)~asc)
survdiff(Surv(time,status)~agegr)

cox <- coxph(Surv(time,status)~sex+treat+ asc+ age)
cox
summary(cox)

cox.strata <- coxph(Surv(time,status)~strata(sex)+treat+ asc+ age)
cox.strata
summary(cox.strata)

model1 <- coxph(Surv(time,status)~sex+ asc+ age)
model1
summary(model1)

model2 <- coxph(Surv(time,status)~asc+ age + sex)
model2
summary(model2)

model3 <- coxph(Surv(time,status)~asc+ age)
model3
summary(model3)

model4 <- coxph(Surv(time,status)~asc)
model4
summary(model4)

