# Libraries
source("Libraries.R")

# fit the KM model for the data 
km.model <- survival::survfit(survival::Surv(time, event) ~ arm, data = IPD.Brahmer.a)

# Call: survfit(formula = survival::Surv(time, event) ~ arm, data = IPD.Brahmer.a)
# 
# n events median 0.95LCL 0.95UCL
# arm=0 290    220   9.94    8.41    11.0
# arm=1 292    188  12.37   10.24    15.3

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

legend(18, 0.95, legend = c("Docetaxel", "Nivolumab"),
       lty = 1, lwd = 2,
       col = c("red", "blue"), bty = "", cex = 0.6)

# log-rank test
survival::survdiff(survival::Surv(time, event)~arm, data = IPD.Brahmer.a)
# Ho: survival in the two groups the same
# H1: survival in the two groups not the same

# survival::survdiff(formula = survival::Surv(time, event) ~ arm, 
#                    data = IPD.Brahmer.a)
# 
# N Observed Expected (O-E)^2/E (O-E)^2/V
# arm=0 290      220      192      4.01      7.79
# arm=1 292      188      216      3.57      7.79
# 
# Chisq= 7.8  on 1 degrees of freedom, p= 0.005 

# Chisq= 7.8 on 1 degrees of freedom, p= 0.005 reject.

# fit coxph model
colnames(IPD.Brahmer.a) <- c("time", "event", "Nivolumab")

cox.model <- survival::coxph(Surv(time, event) ~ Nivolumab ,data = IPD.Brahmer.a)

# check a summary
summary(cox.model)

# Call:
#   survival::coxph(formula = Surv(time, event) ~ Nivolumab, data = IPD.Brahmer.a)
# 
# n= 582, number of events= 408 
# 
# coef exp(coef) se(coef)      z Pr(>|z|)   
# Nivolumab -0.27919   0.75640  0.09997 -2.793  0.00523 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# Nivolumab    0.7564      1.322    0.6218    0.9201
# 
# Concordance= 0.521  (se = 0.014 )
# Likelihood ratio test= 7.83  on 1 df,   p=0.005
# Wald test            = 7.8  on 1 df,   p=0.005
# Score (logrank) test = 7.85  on 1 df,   p=0.005

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

#      pval pval_FH(0,0) pval_FH(0,1) pval_FH(1,1) pval_FH(1,0) pval_APPLE
# 0.4284894    0.3592651    0.4284894    0.1652925    0.3140572  0.2039543

# The w.l.r test is not significant. 

# max combo test
DT <- setDT(IPD.Brahmer.a)

rgs <- list(c(0, 0), c(0, 1), c(1, 1), c(1, 0))

draws <- 1000

combo.wlr(survival = DT$time, cnsr = DT$event, trt = DT$Nivolumab, fparam = list(rgs=rgs,draws=draws))

# $rho
# [1] 1
# 
# $gamma
# [1] 1
# 
# $Zmax
# [1] 1.082861
# 
# $pval
# [1] 0.25
# 
# $hr
# [1] 0.8328269
# 
# $hrL
# [1] 0.5985
# 
# $hrU
# [1] 1.158898
# 
# $hrL.bc
# [1] 0.5754228
# 
# $hrU.bc
# [1] 1.231861

# The results are not significant.
