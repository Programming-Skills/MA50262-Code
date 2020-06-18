# Libraries
source("Libraries.R")

# fit the KM model for the data 
km.model <- survival::survfit(survival::Surv(time, event) ~ arm, data = IPD.Kantarjian.b)

# median survival and CI
km.model

# model summary
summary(km.model)

# Call: survfit(formula = survival::Surv(time, event) ~ arm, data = IPD.Kantarjian.b)

# n events median 0.95LCL 0.95UCL
# arm=0 162     36  12.23    8.60      NA
# arm=1 164    116   5.33    4.43    6.21

# plot the model 
plot(km.model, conf.int = F, xlab = "Time (months)",
     ylab = "%Alive = S(t)", main = "KM-Model",
     col=c("red", "blue"),
     las=1,lwd =2,mark.time = T)

abline(h=0.5, col="black")

legend(18, 0.95, legend = c("Inotuzumab", "Standard"),
       lty = 1, lwd = 2,
       col = c("red", "blue"), bty = "", cex = 0.6)

# log-rank test
survival::survdiff(survival::Surv(time, event)~arm, data = IPD.Kantarjian.b)
# Ho: survival in the two groups the same
# H1: survival in the two groups not the same

# Call:
#   survival::survdiff(formula = survival::Surv(time, event) ~ arm, 
#                      data = IPD.Kantarjian.b)
# 
# N Observed Expected (O-E)^2/E (O-E)^2/V
# arm=0 162       36     55.3      6.74      11.3
# arm=1 164      116     96.7      3.86      11.3

# Chisq= 11.3  on 1 degrees of freedom, p= 8e-04

# Chisq= 11.3  on 1 degrees of freedom, p= 8e-04 reject.

# fit coxph model
colnames(IPD.Kantarjian.b) <- c("time", "event", "Inotuzumab")

cox.model <- survival::coxph(Surv(time, event) ~ Inotuzumab,data = IPD.Kantarjian.b)

# check a summary
summary(cox.model)

# Call:
#   survival::coxph(formula = Surv(time, event) ~ Inotuzumab, data = IPD.Kantarjian.b)
# 
# n= 326, number of events= 152 
# 
# coef exp(coef) se(coef)     z Pr(>|z|)    
# Inotuzumab 0.6496    1.9149   0.1943 3.344 0.000825 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# Inotuzumab     1.915     0.5222     1.309     2.802
# 
# Concordance= 0.566  (se = 0.023 )
# Likelihood ratio test= 12.29  on 1 df,   p=5e-04
# Wald test            = 11.18  on 1 df,   p=8e-04
# Score (logrank) test = 11.55  on 1 df,   p=7e-04

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

# chisq df    p
# Inotuzumab 0.238  1 0.63
# GLOBAL     0.238  1 0.63

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
IPD.Kantarjian.b$Inotuzumab <- ifelse(IPD.Kantarjian.b$Inotuzumab == 0, "control", "experimental")

DT <- setDT(IPD.Kantarjian.b)

wlr.Stat(surv=DT$time, cnsr=DT$event, trt= DT$Inotuzumab,
         fparam=list(rho=c(0,0,1,1), gamma=c(0,1,1,0), wlr='FH(0,1)', APPLE=3))

# pval pval_FH(0,0) pval_FH(0,1) pval_FH(1,1) pval_FH(1,0)   pval_APPLE
#    0            0            0            0            0 1.110223e-16

# max combo test
rgs <- list(c(0, 0), c(0, 1), c(1, 1), c(1, 0))

draws <- 10

combo.wlr(survival = DT$time, cnsr = DT$event, trt = DT$Inotuzumab, fparam = list(rgs=rgs,draws=draws))

# Lower bound: coxph vs Mine 0.1276219 0.1287042 
# Upper Bound: coxph vs Mine 0.2608321 0.2586387 
# Z^2: coxph vs Mine 105.9228 108.2065 
# Call:
#   survdiff(formula = Surv(time, delta) ~ z)
# 
# N Observed Expected (O-E)^2/E (O-E)^2/V
# z=FALSE 162      126       63      63.0       108
# z=TRUE  164       48      111      35.8       108
# 
# Chisq= 108  on 1 degrees of freedom, p= <2e-16 
# $rho
# [1] 0
# 
# $gamma
# [1] 0
# 
# $Zmax
# [1] 10.40223
# 
# $pval
# [1] 0
# 
# $hr
# [1] 0.1824497
# 
# $hrL
# [1] 0.1287042
# 
# $hrU
# [1] 0.2586387
# 
# $hrL.bc
# [1] 0.1361437
# 
# $hrU.bc
# [1] 0.2628318
