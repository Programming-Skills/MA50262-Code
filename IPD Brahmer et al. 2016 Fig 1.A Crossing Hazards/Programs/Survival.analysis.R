source("Libraries.R")

######################################################################
########################## Summary ###################################
######################################################################

# rename columns 
colnames(IPD.Brahmer.a) <- c("time", "event", "Nivolumab")

IPD.Brahmer.a$Nivolumab <- ifelse(IPD.Brahmer.a$Nivolumab == 0, "Docetaxel", "Nivolumab")

IPD.Brahmer.a %>%
  tbl_summary(
    by = Nivolumab,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)")
  ) %>%
  modify_header(stat_by = md("**{level}**<br>N =  {n} ({style_percent(p)}%)")) %>%
  bold_labels() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Nivolumab in Combination With Chemotherapy**") %>% 
  add_p(test = all_continuous() ~ "t.test") %>%
  add_overall()

######################################################################
########################## KM Model ##################################
######################################################################

# fit the KM model for the data 
km.model <- survfit(Surv(time, event) ~ IPD.Brahmer.a$Nivolumab, data = IPD.Brahmer.a)

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

legend(18, 0.95, legend = c("Docetaxel", "Nivolumab"),
       lty = 1, lwd = 2,
       col = c("red", "blue"), bty = "", cex = 0.6)

# log-rank test

# Ho: survival in the two groups the same
# H1: survival in the two groups not the same

survdiff(Surv(time, event)~IPD.Brahmer.a$Nivolumab, data = IPD.Brahmer.a) 

# Chisq= 3.5  on 1 degrees of freedom, p= 0.06 fail to reject.

#########################################################################
########################## Coxph Model ##################################
#########################################################################

colnames(IPD.Brahmer.a) <- c("time", "event", "arm")

# fit coxph model
cox.model <- coxph(Surv(time, event) ~ arm, data = IPD.Brahmer.a) 

# format results into data frame with global p-values
cox.model %>%
  tbl_regression(
    show_single_row = arm,
    label = arm ~ "Nivolumab vs Docetaxel",
    exponentiate = TRUE) %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

colnames(IPD.Brahmer.a) <- c("time", "event", "Nivolumab")

# Schoenfeld plot
ggcoxzph(cox.zph(coxph(Surv(time,event) ~ Nivolumab, data=IPD.Brahmer.a))) 

cox.model <- coxph(Surv(time, event) ~ Nivolumab, data = IPD.Brahmer.a) 

# Schoenfeld plot
par(mfrow=c(1,1))
plot(cox.zph(cox.model), main = "Schoenfeld Individual Test p: 0.002")
abline(h=0, col=2)

# forest plot
ggforest(cox.model, data = IPD.Brahmer.a)

#########################################################################
########################## Weighted Log-Rank Test #######################
#########################################################################

colnames(IPD.Brahmer.a) <- c("time", "event", "arm")

wlr.Stat(survival = IPD.Brahmer.a$time, cnsr = IPD.Brahmer.a$event, trt = IPD.Brahmer.a$arm,
         fparam=list(rho=c(0,0,1,1), gamma=c(0,1,1,0), wlr='FH(0,1)', APPLE=3))

#      pval    pval_FH(0,0) pval_FH(0,1) pval_FH(1,1) pval_FH(1,0) pval_APPLE
# 0.4284894    0.3592651    0.4284894    0.1652925    0.3140572    0.2039543

#########################################################################
########################## Max-Combo Test ###############################
#########################################################################

# max combo test
rgs <- list(c(0, 0), c(0, 1), c(1, 1), c(1, 0))

draws <- 10

IPD.Brahmer.a$arm <- ifelse(IPD.Brahmer.a$arm == 0, "control", "experimental")

result.mc <- nphsim::combo.wlr(survival = IPD.Brahmer.a$time, cnsr = IPD.Brahmer.a$event, trt = IPD.Brahmer.a$arm, fparam = list(rgs=rgs,draws=draws))

unlist(result.mc)

# rho  gamma  Zmax       pval      hr        hrL       hrU       hrL.bc    hrU.bc 
# 1    1      1.0828606 0.4000000 0.8328269 0.5985000 1.1588983 0.6554384 1.0417779 
