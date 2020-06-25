source("Libraries.R")

######################################################################
########################## Summary ###################################
######################################################################

# rename columns 
colnames(IPD.Ohtsu) <- c("time", "event", "Bevacizumab")

IPD.Ohtsu$Bevacizumab <- ifelse(IPD.Ohtsu$Bevacizumab == 0, "Placebo", "Bevacizumab")

IPD.Ohtsu %>%
  tbl_summary(
    by = Bevacizumab,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)")
  ) %>%
  modify_header(stat_by = md("**{level}**<br>N =  {n} ({style_percent(p)}%)")) %>%
  bold_labels() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Bevacizumab in Combination With Chemotherapy**") %>% 
  add_p(test = all_continuous() ~ "t.test") %>%
  add_overall()

######################################################################
########################## KM Model ##################################
######################################################################

# fit the KM model for the data 
km.model <- survfit(Surv(time, event) ~ IPD.Ohtsu$Bevacizumab, data = IPD.Ohtsu)

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

legend(18, 0.95, legend = c("Bevacizumab", "Placebo"),
       lty = 1, lwd = 2,
       col = c("red", "blue"), bty = "", cex = 0.6)

# log-rank test

# Ho: survival in the two groups the same
# H1: survival in the two groups not the same

survdiff(Surv(time, event)~IPD.Ohtsu$Bevacizumab, data = IPD.Ohtsu) 

#########################################################################
########################## Coxph Model ##################################
#########################################################################

colnames(IPD.Ohtsu) <- c("time", "event", "arm")

# fit coxph model
cox.model <- coxph(Surv(time, event) ~ arm, data = IPD.Ohtsu) 

# format results into data frame with global p-values
cox.model %>%
  tbl_regression(
    show_single_row = arm,
    label = arm ~ "Bevacizumab vs Placebo",
    exponentiate = TRUE) %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()


colnames(IPD.Ohtsu) <- c("time", "event", "Bevacizumab")

# Schoenfeld test
ggcoxzph(cox.zph(coxph(Surv(time,event) ~ Bevacizumab, data=IPD.Ohtsu))) 

cox.model <- coxph(Surv(time, event) ~ Bevacizumab, data = IPD.Ohtsu) 

# Schoenfeld test
par(mfrow=c(1,1))
plot(cox.zph(cox.model), main = "Schoenfeld Individual Test p: 1.879e-26")
abline(h=0, col=2)

# forest plot
ggforest(cox.model, data = IPD.Ohtsu)

#########################################################################
########################## Weighted  Coxph Model ########################
#########################################################################

# weighted estimation of average hazard ratio
coxphw.model <- coxphw(Surv(time, event) ~ arm, data = IPD.Ohtsu, template = "AHR")
summary(coxphw.model)
coxphw.model$cov.lw # robust covariance
coxphw.model$cov.ls # Lin-Sasieni covariance

# Weights used by weighted Cox regression are plotted against time
plot(coxphw.model, 
     main="Weights vs Time")

#########################################################################
########################## Weighted Log-Rank Test #######################
#########################################################################

IPD.Ohtsu$event <- ifelse(IPD.Ohtsu$event == 0, 1, 0)

colnames(IPD.Ohtsu) <- c("time", "event", "arm")

result <- wlr.Stat(surv=IPD.Ohtsu$time, 
                   cnsr=IPD.Ohtsu$event, 
                   trt= IPD.Ohtsu$arm, 
                   fparam=list(rho=c(0,0,1,1), gamma=c(0,1,1,0), wlr='FH(0,1)', APPLE=NULL))

result %>% kable()

# pval	    pval_FH(0,0)	pval_FH(0,1)	pval_FH(1,1)	pval_FH(1,0)
# 0.4897185	0.0609406	    0.4897185	    0.2393201	    0.0130434

#########################################################################
########################## Max-Combo Test ###############################
#########################################################################

IPD.Ohtsu$event <- ifelse(IPD.Ohtsu$event == 0, 1, 0)

IPD.Ohtsu$arm <- ifelse(IPD.Ohtsu$arm == 0, "control", "experimental")

rgs <- list(c(0, 0), c(0, 1), c(1, 0), c(1, 1))

draws <- 10

result.mc <- nphsim::combo.wlr(survival = IPD.Ohtsu$time, 
                               cnsr = IPD.Ohtsu$event, 
                               trt = IPD.Ohtsu$arm, 
                               fparam = list(rgs=rgs,draws=draws))

result.mc <- lapply(result.mc, function(x) round(x, digits = 3))

as.data.frame(result.mc) %>% kable()

# pval	  tau	est	    estlb	  estub
# 0.02103	22	1.05979	0.03802	2.08156

########################################################################
############################### RMST ###################################
########################################################################

# nphsim package
IPD.Ohtsu$arm <- ifelse(IPD.Ohtsu$arm == 0, "control", "experimental")

IPD.Ohtsu$event <- ifelse(IPD.Ohtsu$event == 0, 1, 0)

result.rmst <- rmst.Stat(survival = IPD.Ohtsu$time, 
                         cnsr = IPD.Ohtsu$event, 
                         trt = IPD.Ohtsu$arm, 
                         stra = NULL, 
                         fparam = 22)

result.rmst <- lapply(result.rmst, function(x) round(x, digits = 4))

as.data.frame(result.rmst) %>% kable()

# pval	  tau	est	    estlb	  estub
# 0.02103	22	1.05979	0.03802	2.08156

# rmst2 package
result.rmst2 = rmst2(time = IPD.Ohtsu$time, 
                     status = IPD.Ohtsu$event, 
                     arm = IPD.Ohtsu$arm, 
                     tau = 22)

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

