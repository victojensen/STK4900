# Cox regression and Cox strata

cirrhosis <-  read.table("https://www.uio.no/studier/emner/matnat/math/STK4900/data/cirrhosis.txt",header=TRUE)

library(survival)

survpred <- survfit(Surv(time,status)~1, conf.type="none") 
summary(survpred,xlab="Time(days)",ylab="Survival")

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
