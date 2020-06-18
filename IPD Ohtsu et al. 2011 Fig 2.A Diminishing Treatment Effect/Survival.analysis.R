# Libraries
source("Libraries.R")

# fit the KM model for the data 
km.model <- survival::survfit(survival::Surv(time, event) ~ arm, data = IPD.Ohtsu)

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
survival::survdiff(survival::Surv(time, event)~arm, data = IPD.Ohtsu)
# Ho: survival in the two groups the same
# H1: survival in the two groups not the same

# Chisq= 3.5  on 1 degrees of freedom, p= 0.06 fail to reject.

# fit coxph model
colnames(IPD.Ohtsu) <- c("time", "event", "Bevacizumab")

cox.model <- survival::coxph(Surv(time, event) ~ Bevacizumab,data = IPD.Ohtsu)

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
IPD.Ohtsu$Bevacizumab <- ifelse(IPD.Ohtsu$Bevacizumab == 0, "control", "experimental")

DT <- setDT(IPD.Ohtsu)

wlr.Stat(surv=DT$time, cnsr=DT$event, trt= DT$Bevacizumab,
         fparam=list(rho=c(0,0,1,1), gamma=c(0,1,1,0), wlr='FH(0,1)', APPLE=3))

#       pval pval_FH(0,0) pval_FH(0,1) pval_FH(1,1) pval_FH(1,0) pval_APPLE
# 0.03353598    0.1973005   0.03353598    0.3359253    0.4478928  0.2756842

# max combo test
rgs <- list(c(0, 0), c(0, 1), c(1, 1), c(1, 0))

draws <- 1000 

combo.wlr(survival = DT$time, cnsr = DT$event, trt = DT$Bevacizumab, fparam = list(rgs=rgs,draws=draws))

# $rho
# [1] 0
# 
# $gamma
# [1] 1
# 
# $Zmax
# [1] 1.831192
# 
# $pval
# [1] 0.081
# 
# $hr
# [1] 0.7630395
# 
# $hrL
# [1] 0.572735
# 
# $hrU
# [1] 1.016577
# 
# $hrL.bc
# [1] 0.5529727
# 
# $hrU.bc
# [1] 1.064516
