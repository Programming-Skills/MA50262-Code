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

IPD.Brahmer.a$event <- ifelse(IPD.Brahmer.a$event == 0, 1, 0)

colnames(IPD.Brahmer.a) <- c("time", "event", "arm")

result <- wlr.Stat(surv=IPD.Brahmer.a$time, 
                   cnsr=IPD.Brahmer.a$event, 
                   trt= IPD.Brahmer.a$arm, 
                   fparam=list(rho=c(0,0,1,1), 
                               gamma=c(0,1,1,0), 
                               wlr='FH(0,1)', 
                               APPLE=NULL))

result %>% kable()

# pval	    pval_FH(0,0)	pval_FH(0,1)	pval_FH(1,1)	pval_FH(1,0)
# 4.66e-05	0.0025083	    4.66e-05	    4.52e-05	    0.0372977

#########################################################################
########################## Max-Combo Test ###############################
#########################################################################

IPD.Brahmer.a$event <- ifelse(IPD.Brahmer.a$event == 0, 1, 0)

IPD.Brahmer.a$arm <- ifelse(IPD.Brahmer.a$arm == 0, "control", "experimental")

rgs <- list(c(0, 0), c(0, 1), c(1, 1), c(1, 0))

draws <- 10

result.mc <- nphsim::combo.wlr(survival = IPD.Brahmer.a$time, 
                               cnsr = IPD.Brahmer.a$event, 
                               trt = IPD.Brahmer.a$arm, 
                               fparam = list(rgs=rgs,draws=draws))

result.mc <- lapply(result.mc, function(x) round(x, digits = 3))

as.data.frame(result.mc) %>% kable()

# rho	gamma	Zmax	pval	  hr	  hrL	  hrU	  hrL.bc	hrU.bc
# 0	  1	    4.032	0	      0.631	0.502	0.792	0.564	  0.759 

########################################################################
############################### RMST ###################################
########################################################################

# nphsim package
IPD.Brahmer.a$arm <- ifelse(IPD.Brahmer.a$arm == 0, "control", "experimental")

IPD.Brahmer.a$event <- ifelse(IPD.Brahmer.a$event == 0, 1, 0)

result.rmst <- rmst.Stat(survival = IPD.Brahmer.a$time, 
                         cnsr = IPD.Brahmer.a$event, 
                         trt = IPD.Brahmer.a$arm, 
                         stra = NULL, 
                         fparam = 25)

result.rmst <- lapply(result.rmst, function(x) round(x, digits = 4))

as.data.frame(result.rmst) %>% kable()

# rmst2 package
result.rmst2 = rmst2(time = IPD.Brahmer.a$time, 
                     status = IPD.Brahmer.a$event, 
                     arm = IPD.Brahmer.a$arm, 
                     tau = 25)

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

