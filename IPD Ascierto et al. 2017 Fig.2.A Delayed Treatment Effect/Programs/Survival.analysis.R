# Libraries
source("Libraries.R")

######################################################################
########################## Summary ###################################
######################################################################

# rename columns 
colnames(IPD.ascierto.2.a) <- c("time", "event", "10mg")

IPD.ascierto.2.a$`10mg` <- ifelse(IPD.ascierto.2.a$`10mg` == 0, "3mg", "10mg")

IPD.ascierto.2.a %>%
  tbl_summary(
    by = `10mg`,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)")
  ) %>%
  modify_header(stat_by = md("**{level}**<br>N =  {n} ({style_percent(p)}%)")) %>%
  bold_labels() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Ipilimumab 10 mg/kg versus ipilimumab 3 mg/kg**") %>% 
  add_p(test = all_continuous() ~ "t.test") %>%
  add_overall()

######################################################################
########################## KM Model ##################################
######################################################################

# fit the KM model for the data 
km.model <- survfit(Surv(time, event) ~ IPD.ascierto.2.a$`10mg`, data = IPD.ascierto.2.a)

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

legend(18, 0.95, legend = c("3mg", "10mg"),
       lty = 1, lwd = 2,
       col = c("red", "blue"), bty = "", cex = 0.6)

# log-rank test

# Ho: survival in the two groups the same
# H1: survival in the two groups not the same

survdiff(Surv(time, event)~IPD.ascierto.2.a$`10mg`, data = IPD.ascierto.2.a) 

#########################################################################
########################## Coxph Model ##################################
#########################################################################

IPD.ascierto.2.a$`10mg` <- ifelse(IPD.ascierto.2.a$`10mg` == "3mg", 0,1)

colnames(IPD.ascierto.2.a) <- c("time", "event", "arm")

# fit coxph model
cox.model <- coxph(Surv(time, event) ~ arm, data = IPD.ascierto.2.a) 

# check a summary
summary(cox.model)

# format results into data frame with global p-values
cox.model %>%
  tbl_regression(
    show_single_row = arm,
    label = arm ~ "10mg vs 3mg",
    exponentiate = TRUE) %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()


colnames(IPD.ascierto.2.a) <- c("time", "event", "10mg")

ggcoxzph(cox.zph(coxph(Surv(time,event) ~ `10mg`, data=IPD.ascierto.2.a))) 

# plot of "changes in b over time"
cox.model <- coxph(Surv(time, event) ~ `10mg`, data = IPD.ascierto.2.a) 

par(mfrow=c(1,1))
plot(cox.zph(cox.model), main = "Schoenfeld Individual Test p: 0.1812")
abline(h=0, col=2)

# forest plot
ggforest(cox.model, data = IPD.ascierto.2.a)

#########################################################################
########################## Weighted Log-Rank Test #######################
#########################################################################
colnames(IPD.ascierto.2.a) <- c("time", "event", "arm")

DT <- setDT(IPD.ascierto.2.a)

wlr.Stat(surv=DT$time, cnsr=DT$event, trt= DT$arm,
         fparam=list(rho=c(0,0,1,1), gamma=c(0,1,1,0), wlr='FH(0,1)', APPLE=3))

#  pval pval_FH(0,0) pval_FH(0,1) pval_FH(1,1) pval_FH(1,0)   pval_APPLE
#  0    0.001753487            0  1.74305e-14   0.06047079 0.0006565487

#########################################################################
########################## Max-Combo Test ###############################
#########################################################################

# max combo test
rgs <- list(c(0, 0), c(0, 1), c(1, 1), c(1, 0))

draws <- 10

DT$arm <- ifelse(DT$arm == 0, "control", "experimental")

result.mc <- nphsim::combo.wlr(surv=DT$time, cnsr=DT$event, trt= DT$arm, fparam = list(rgs=rgs,draws=draws))

unlist(result.mc)

# rho     gamma      Zmax      pval        hr       hrL       hrU    hrL.bc    hrU.bc 
# 0.0000000 1.0000000 8.3618583 0.0000000 0.2523997 0.2011542 0.3167004 0.2149434 0.2846335 
