# Libraries
source("Libraries.R")

######################################################################
########################## Summary ###################################
######################################################################

# rename columns 
colnames(IPD.Mok.C) <- c("time", "event", "Gefitinib")

IPD.Mok.C$Gefitinib <- ifelse(IPD.Mok.C$Gefitinib == 0, "Carboplatin", "Gefitinib")

IPD.Mok.C %>%
  tbl_summary(
    by = Gefitinib,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)")
  ) %>%
  modify_header(stat_by = md("**{level}**<br>N =  {n} ({style_percent(p)}%)")) %>%
  bold_labels() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Gefitinib vs Carboplatin-Paclitaxel**") %>% 
  add_p(test = all_continuous() ~ "t.test") %>%
  add_overall()

######################################################################
########################## KM Model ##################################
######################################################################

# fit the KM model for the data 
km.model <- survfit(Surv(time, event) ~ IPD.Mok.C$Gefitinib, data = IPD.Mok.C)

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

survdiff(Surv(time, event)~IPD.Mok.C$Gefitinib, data = IPD.Mok.C) 

#########################################################################
########################## Coxph Model ##################################
#########################################################################

colnames(IPD.Mok.C) <- c("time", "event", "arm")

# fit coxph model
cox.model <- coxph(Surv(time, event) ~ arm, data = IPD.Mok.C) 

# format results into data frame with global p-values
cox.model %>%
  tbl_regression(
    show_single_row = arm,
    label = arm ~ "Gefitinib vs Carboplatin-Paclitaxel",
    exponentiate = TRUE) %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

colnames(IPD.Mok.C) <- c("time", "event", "Gefitinib")

ggcoxzph(cox.zph(coxph(Surv(time,event) ~ Gefitinib, data=IPD.Mok.C))) 

# plot of "changes in b over time"
cox.model <- coxph(Surv(time, event) ~ Gefitinib, data = IPD.Mok.C) 

par(mfrow=c(1,1))
plot(cox.zph(cox.model), main = "Schoenfeld Individual Test p: 2e-09")
abline(h=0, col=2)

# forest plot
ggforest(cox.model, data = IPD.Mok.C)

#########################################################################
########################## Weighted Log-Rank Test #######################
#########################################################################

wlr.Stat(survival = IPD.Mok.C$time, cnsr = IPD.Mok.C$event, trt = IPD.Mok.C$arm,
         fparam=list(rho=c(0,0,1,1), gamma=c(0,1,1,0), wlr='FH(0,1)', APPLE=3))

#      pval	pval_FH(0,0)	pval_FH(0,1)	pval_FH(1,1)	pval_FH(1,0)	pval_APPLE
# 0.3768385	0.0085894	   0.3768385	   0.5382168	    0.0028161	    0.1774779

#########################################################################
########################## Max-Combo Test ###############################
#########################################################################

rgs <- list(c(0, 0), c(0, 1), c(1, 1), c(1, 0))

draws <- 10

IPD.Mok.C$arm <- ifelse(IPD.Mok.C$arm == 0, "control", "experimental")

result.mc <- nphsim::combo.wlr(survival = IPD.Mok.C$time, cnsr = IPD.Mok.C$event, trt = IPD.Mok.C$arm, fparam = list(rgs=rgs,draws=draws))

unlist(result.mc)

# rho      gamma       Zmax       pval         hr        hrL        hrU     hrL.bc     hrU.bc 
# 1.00000000 0.00000000 5.99783044 0.00000000 0.08054584 0.02798934 0.23178939 0.06161595 0.11150244 
