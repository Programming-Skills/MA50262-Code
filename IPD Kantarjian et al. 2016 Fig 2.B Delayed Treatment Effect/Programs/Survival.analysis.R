source("Libraries.R")

######################################################################
########################## Summary ###################################
######################################################################

# rename columns 
colnames(IPD.Kantarjian.b) <- c("time", "event", "NInotuzumab")

IPD.Kantarjian.b$Nivolumab <- ifelse(IPD.Kantarjian.b$Nivolumab == 0, "Standard", "Inotuzumab")

IPD.Kantarjian.b %>%
  tbl_summary(
    by = Nivolumab,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)")
  ) %>%
  modify_header(stat_by = md("**{level}**<br>N =  {n} ({style_percent(p)}%)")) %>%
  bold_labels() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Inotuzumab vs Standard**") %>% 
  add_p(test = all_continuous() ~ "t.test") %>%
  add_overall()

######################################################################
########################## KM Model ##################################
######################################################################

# fit the KM model for the data 
km.model <- survfit(Surv(time, event) ~ IPD.Kantarjian.b$Inotuzumab, data = IPD.Kantarjian.b)

# median survival and CI
km.model 

# model summary
summary(km.model)

# plot the model 
plot(km.model, conf.int = F, xlab = "Time (months)",
     ylab = "%Alive = S(t)", main = "Kaplan-Meier Curves",
     col=c("red", "blue"),
     las=1,lwd =2,mark.time = T)

abline(h=0.5, col="black")

legend(18, 0.95, legend = c("Standard", "Inotuzumab"),
       lty = 1, lwd = 2,
       col = c("red", "blue"), bty = "", cex = 0.6)

# log-rank test

# Ho: survival in the two groups the same
# H1: survival in the two groups not the same

survdiff(Surv(time, event)~IPD.Kantarjian.b$Inotuzumab, data = IPD.Kantarjian.b) 

# Chisq= 3.5  on 1 degrees of freedom, p= 0.06 fail to reject.

#########################################################################
########################## Coxph Model ##################################
#########################################################################

colnames(IPD.Kantarjian.b) <- c("time", "event", "arm")

# fit coxph model
cox.model <- coxph(Surv(time, event) ~ arm, data = IPD.Kantarjian.b) 

# format results into data frame with global p-values
cox.model %>%
  tbl_regression(
    show_single_row = arm,
    label = arm ~ "Inotuzumab vs Standard",
    exponentiate = TRUE) %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

colnames(IPD.Kantarjian.b) <- c("time", "event", "Inotuzumab")

# Schoenfeld plot
ggcoxzph(cox.zph(coxph(Surv(time,event) ~ Inotuzumab, data=IPD.Kantarjian.b))) 

cox.model <- coxph(Surv(time, event) ~ Inotuzumab, data = IPD.Kantarjian.b) 

# Schoenfeld plot
par(mfrow=c(1,1))
plot(cox.zph(cox.model), main = "Schoenfeld Individual Test p: 0.002")
abline(h=0, col=2)

# forest plot
ggforest(cox.model, data = IPD.Kantarjian.b)

#########################################################################
########################## Weighted  Coxph Model ########################
#########################################################################

# weighted estimation of average hazard ratio
coxphw.model <- coxphw(Surv(time, event) ~ arm, data = IPD.Mok.A, template = "AHR")
summary(coxphw.model)
coxphw.model$cov.lw # robust covariance
coxphw.model$cov.ls # Lin-Sasieni covariance

# Weights used by weighted Cox regression are plotted against time
plot(coxphw.model, 
     main="Weights vs Time")

#########################################################################
########################## Weighted Log-Rank Test #######################
#########################################################################

colnames(IPD.Kantarjian.b) <- c("time", "event", "arm")

wlr.Stat(survival = IPD.Kantarjian.b$time, cnsr = IPD.Kantarjian.b$event, trt = IPD.Kantarjian.b$arm,
         fparam=list(rho=c(0,0,1,1), gamma=c(0,1,1,0), wlr='FH(0,1)', APPLE=3))

# pval pval_FH(0,0) pval_FH(0,1) pval_FH(1,1) pval_FH(1,0)   pval_APPLE
#   0            0            0            0            0 1.110223e-16

#########################################################################
########################## Plots of Weights #############################
#########################################################################

# Pooled Survival Function S(t-) or 1-S(t-) = S(t)
S <- survfit(Surv(time, event) ~ 1, data = IPD.Kantarjian.b)

# FH(1,0)  1-S(t-) vs t (Prentice-Wilcoxon)
plot(x = S$time, y = S$surv,
     main = "FH(1,0)  1-S(t-) vs Time (Prentice-Wilcoxon)", 
     xlab = "Time",
     ylab = "1-S(t-)",
     col = "blue")

# FH(0,1) S(t) against t
plot(x = S$time, y = (1-S$surv),
     main = "FH(0,1) S(t) vs Time", 
     xlab = "Time",
     ylab = "S(t)",
     col = "blue")

# Log.Rank 
plot(x = S$time, 
     y = rep(1, times = length(S$time)),
     main = "FH(0,0) Constant Function One Vs Time", 
     xlab = "Time",
     ylab = "Constant Function 1",
     col = "blue")

# FH(1,1) S(t)*(1-S(t))
plot(x = S$time, 
     y = (S$surv)*(1-(S$surv)), 
     main = "FH(1,1) S(t)*(1-S(t))", 
     xlab = "Time",
     ylab = "S(t)*(1-S(t)",
     col = "blue")

#########################################################################
########################## Max-Combo Test ###############################
#########################################################################

# max combo test
rgs <- list(c(0, 0), c(0, 1), c(1, 1), c(1, 0))

draws <- 10

IPD.Kantarjian.b$arm <- ifelse(IPD.Kantarjian.b$arm == 0, "control", "experimental")

result.mc <- nphsim::combo.wlr(survival = IPD.Kantarjian.b$time, cnsr = IPD.Kantarjian.b$event, trt = IPD.Kantarjian.b$arm, fparam = list(rgs=rgs,draws=draws))

unlist(result.mc)

# rho      gamma       Zmax       pval         hr        hrL        hrU     hrL.bc     hrU.bc 
# 0.0000000  0.0000000 10.4022343  0.0000000  0.1824497  0.1287042  0.2586387  0.1366868  0.2328065 

