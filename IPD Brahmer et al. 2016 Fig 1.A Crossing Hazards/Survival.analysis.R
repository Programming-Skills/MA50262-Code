# Libraries
source("Libraries.R")

# fit the KM model for the data 
km.model <- survival::survfit(survival::Surv(time, event) ~ arm, data = IPD.Brahmer.a)

# median survival and CI
km.model

# model summary
summary(km.model)

# plot the model 
plot(km.model, conf.int = F, xlab = "Time (months)",
     ylab = "%Alive = S(t)", main = "KM-Model",
     col=c("red", "blue"),
     las=1,lwd =2,mark.time = T)

abline(h=0.5, col="black")

legend(18, 0.95, legend = c("arm=0", "arm=1"),
       lty = 1, lwd = 2,
       col = c("red", "blue"), bty = "", cex = 0.6)

# log-rank test
survival::survdiff(survival::Surv(time, event)~arm, data = IPD.Brahmer.a)
# Ho: survival in the two groups the same
# H1: survival in the two groups not the same

# Chisq= 3.5  on 1 degrees of freedom, p= 0.005 reject.

# fit coxph model
colnames(IPD.Brahmer.a) <- c("time", "event", "Nivolumab")

cox.model <- survival::coxph(Surv(time, event) ~ Nivolumab ,data = IPD.Brahmer.a)

# baseline hazard is unspeficied in the Cox model.

# can't estimate the survival with the coxph model
# as we are not estimating the intercept so can't
# estimate the hazard, so in turn we can't estimate
# the survival function.

# can estimate the HR. coef is the model coefficient.
# se(coef) is the se of coef. and the p-value for the 
# test that the coef is actually zero.

# exp(coef) is the HR. Here HR is 0.8496: At a given 
# instant in time a patient recieving the 10mg treatment
# is 0.85 times as likely to die as a patient who is on 
# 3mg treatment.

# if we subtract 1 from the HR we can interpret this as 
# a percentage change. At a given instant a patient on the
# 10mg tratment is 15% less likely to die than those patients 
# on the 3mg dose. 

# we are 95% confident the true HR lies between  0.7166 and 1.007.
# exp(-coef) is 1/exp(coef) which is the HR of the 3mg treatment
# relative to the 10mg treatment group. 

# A patient on the 3mg dose is 1.177 times as likely to die
# as a patient on the 10mg dose.

# check a summary
summary(cox.model)

# summary(result.cox)

# broom::tidy(
#   result.cox, 
#   exp = TRUE
# ) %>% 
#   kable()

cox.model %>% 
  gtsummary::tbl_regression(exp = TRUE) 

# checking proportional hazards using Schoenfeld test for PH
# Ho: Hazards are proportional.
# Ha: Hazards are not proportional.
# will return test for each X, and for overall model

# tests if the coef for variable(X) changes over time.
cox.zph(cox.model) 

# plot of "changes in b over time"
par(mfrow=c(1,1))
plot(cox.zph(cox.model))
abline(h=0, col=2)

# solid line: if we allow the coef for treatment to change over
# time (i.e allow the HR's to change over time) how much of a change 
# would we see. 

# a change of zero would mean there is no change.
# the confidence bands are in the red line the majority
# of the time.

# forest plot
ggforest(cox.model)

# weighted log-rank test
IPD.Brahmer.a$Nivolumab <- ifelse(IPD.Brahmer.a$Nivolumab == 0, "control", "experimental")

DT <- setDT(IPD.Brahmer.a)

wlr.Stat(surv=DT$time, cnsr=DT$event, trt= DT$Nivolumab,
         fparam=list(rho=c(0,0,1,1), gamma=c(0,1,1,0), wlr='FH(0,1)', APPLE=3))

# max combo test
DT <- setDT(IPD.Brahmer.a)

rgs <- list(c(0, 0), c(0, 1), c(1, 1), c(1, 0))

draws <- 1000

combo.wlr(survival = DT$time, cnsr = DT$event, trt = DT$Nivolumab, fparam = list(rgs=rgs,draws=draws))
