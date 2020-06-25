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
########################## Weighted  Coxph Model ########################
#########################################################################

# weighted estimation of average hazard ratio
coxphw.model <- coxphw(Surv(time, event) ~ arm, data = IPD.Mok.C, template = "AHR")
summary(coxphw.model)
coxphw.model$cov.lw # robust covariance
coxphw.model$cov.ls # Lin-Sasieni covariance

# Weights used by weighted Cox regression are plotted against time
plot(coxphw.model, 
     main="Weights vs Time")

#########################################################################
########################## Weighted Log-Rank Test #######################
#########################################################################

IPD.Mok.C$event <- ifelse(IPD.Mok.C$event == 0, 1, 0)

colnames(IPD.Mok.C) <- c("time", "event", "arm")

result <- wlr.Stat(surv=IPD.Mok.C$time, 
                   cnsr=IPD.Mok.C$event, 
                   trt= IPD.Mok.C$arm==0, 
                   fparam=list(rho=c(0,0,1,1), gamma=c(0,1,1,0), wlr='FH(0,1)', APPLE=NULL))

result %>% kable()

# pval	    pval_FH(0,0)	pval_FH(0,1)	pval_FH(1,1)	pval_FH(1,0)
# 0.0007766	0	            0.0007766	    0	            0

#########################################################################
########################## Max-Combo Test ###############################
#########################################################################

rgs <- list(c(0, 0), c(0, 1), c(1, 1), c(1, 0))

draws <- 10

IPD.Mok.C$event <- ifelse(IPD.Mok.C$event == 0, 1, 0)

IPD.Mok.C$arm <- ifelse(IPD.Mok.C$arm == 0, "control", "experimental")

result.mc <- nphsim::combo.wlr(survival = IPD.Mok.C$time, 
                               cnsr = IPD.Mok.C$event, 
                               trt = IPD.Mok.C$arm, 
                               fparam = list(rgs=rgs,draws=draws))

result.mc <- lapply(result.mc, function(x) round(x, digits = 3))

as.data.frame(result.mc) %>% kable()

# rho	gamma	Zmax	  pval	hr	  hrL	  hrU	  hrL.bc	hrU.bc
# 0	  1	    -3.254	1	    1.85	1.263	2.71	1.215	  2.468

########################################################################
############################### RMST ###################################
########################################################################

# nphsim package
IPD.Mok.C$arm <- ifelse(IPD.Mok.C$arm == 0, "control", "experimental")

IPD.Mok.C$event <- ifelse(IPD.Mok.C$event == 0, 1, 0)

result.rmst <- rmst.Stat(survival = IPD.Mok.C$time, 
                         cnsr = IPD.Mok.C$event, 
                         trt = IPD.Mok.C$arm, 
                         stra = NULL, 
                         fparam = 12)

result.rmst <- lapply(result.rmst, function(x) round(x, digits = 4))

as.data.frame(result.rmst) %>% kable()

# rmst2 package
result.rmst2 = rmst2(time = IPD.Mok.C$time, 
                     status = IPD.Mok.C$event, 
                     arm = IPD.Mok.C$arm, 
                     tau = 12)

result.rmst2

# plot
plot(
  result.rmst2,
  xlab = "Months",
  ylab = "Probability",
  col = "black",
  col.RMST = "#E7B800", 
  col.RMTL = "#2E9FDF",
  density = 80,
  angle = 85
)


