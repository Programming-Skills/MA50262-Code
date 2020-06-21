# Libraries
source("Libraries.R")

######################################################################
########################## Summary ###################################
######################################################################

# rename columns 
colnames(IPD.Rittmeyer) <- c("time", "event", "Atezolizumab")

IPD.Rittmeyer$Atezolizumab <- ifelse(IPD.Rittmeyer$Atezolizumab == 0, "Docetaxel", "Atezolizumab")

IPD.Rittmeyer %>%
  tbl_summary(
    by = Atezolizumab,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)")
  ) %>%
  modify_header(stat_by = md("**{level}**<br>N =  {n} ({style_percent(p)}%)")) %>%
  bold_labels() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Atezolizumab versus Docetaxel**") %>% 
  add_p(test = all_continuous() ~ "t.test") %>%
  add_overall()

######################################################################
########################## KM Model ##################################
######################################################################

# fit the KM model for the data 
km.model <- survfit(Surv(time, event) ~ IPD.Rittmeyer$Atezolizumab, data = IPD.Rittmeyer)

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

legend(18, 0.95, legend = c("Docetaxel", "Atezolizumab"),
       lty = 1, lwd = 2,
       col = c("red", "blue"), bty = "", cex = 0.6)

# log-rank test

# Ho: survival in the two groups the same
# H1: survival in the two groups not the same

survdiff(Surv(time, event)~IPD.Rittmeyer$Atezolizumab, data = IPD.Rittmeyer) 

#########################################################################
########################## Coxph Model ##################################
#########################################################################

IPD.Rittmeyer$Atezolizumab <- ifelse(IPD.Rittmeyer$Atezolizumab == "Docetaxel", 0,1)

colnames(IPD.Rittmeyer) <- c("time", "event", "arm")

# fit coxph model
cox.model <- coxph(Surv(time, event) ~ arm, data = IPD.Rittmeyer) 

# check a summary
summary(cox.model)

# format results into data frame with global p-values
cox.model %>%
  tbl_regression(
    show_single_row = arm,
    label = arm ~ "Atezolizumab vs Docetaxel",
    exponentiate = TRUE) %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

colnames(IPD.Rittmeyer) <- c("time", "event", "Atezolizumab")

# Schoenfeld plot
ggcoxzph(cox.zph(coxph(Surv(time,event) ~ Atezolizumab, data=IPD.Rittmeyer))) 

cox.model <- coxph(Surv(time, event) ~ Atezolizumab, data = IPD.Rittmeyer) 

# Schoenfeld plot
par(mfrow=c(1,1))
plot(cox.zph(cox.model), main = "Schoenfeld Individual Test p: 0.1812")
abline(h=0, col=2)

# forest plot
ggforest(cox.model, data = IPD.Rittmeyer)

#########################################################################
########################## Weighted Log-Rank Test #######################
#########################################################################

wlr.Stat(survival = IPD.Rittmeyer$time, cnsr = IPD.Rittmeyer$event, trt = IPD.Rittmeyer$arm,
         fparam=list(rho=c(0,0,1,1), gamma=c(0,1,1,0), wlr='FH(0,1)', APPLE=3))

# pval        pval_FH(0,0) pval_FH(0,1) pval_FH(1,1) pval_FH(1,0) pval_APPLE
# 0.375714   0.05402906     0.375714    0.3806117   0.01043189  0.3760572

#########################################################################
########################## Max-Combo Test ###############################
#########################################################################

rgs <- list(c(0, 0), c(0, 1), c(1, 1), c(1, 0))

draws <- 10

IPD.Rittmeyer$arm <- ifelse(IPD.Rittmeyer$arm == 0, "control", "experimental")

result.mc <- nphsim::combo.wlr(survival = IPD.Rittmeyer$time, cnsr = IPD.Rittmeyer$event, trt = IPD.Rittmeyer$arm, fparam = list(rgs=rgs,draws=draws))

unlist(result.mc)

# rho     gamma      Zmax      pval        hr       hrL       hrU    hrL.bc    hrU.bc 
# 1.0000000 0.0000000 2.0412027 0.0000000 0.7609729 0.5857551 0.9886040 0.7067623 0.8663323 
