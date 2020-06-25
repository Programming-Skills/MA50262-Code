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
########################## Weighted  Coxph Model ########################
#########################################################################

# weighted estimation of average hazard ratio
coxphw.model <- coxphw(Surv(time, event) ~ arm, data = IPD.ascierto.2.a, template = "AHR")
summary(coxphw.model)
coxphw.model$cov.lw # robust covariance
coxphw.model$cov.ls # Lin-Sasieni covariance

# Weights used by weighted Cox regression are plotted against time
plot(coxphw.model, 
     main="Weights vs Time")

#########################################################################
########################## Weighted Log-Rank Test #######################
#########################################################################

IPD.ascierto.2.a$event <- ifelse(IPD.ascierto.2.a$event == 0, 1, 0)

colnames(IPD.ascierto.2.a) <- c("time", "event", "arm")

result <- wlr.Stat(surv=IPD.ascierto.2.a$time, 
                   cnsr=IPD.ascierto.2.a$event,
                   trt= IPD.ascierto.2.a$arm, 
                   fparam=list(rho=c(0,0,1,1), gamma=c(0,1,1,0), wlr='FH(0,1)', APPLE=NULL))

result %>% kable()

# pval	    pval_FH(0,0)	pval_FH(0,1)	pval_FH(1,1)	pval_FH(1,0)
# 0.0108167	0.0307526	    0.0108167	    0.0071524	    0.0928615

#########################################################################
########################## Max-Combo Test ###############################
#########################################################################

rgs <- list(c(0, 0), c(0, 1), c(1, 1), c(1, 0))

draws <- 10

IPD.ascierto.2.a$event <- ifelse(IPD.ascierto.2.a$event == 0, 1, 0)

IPD.ascierto.2.a$arm <- ifelse(IPD.ascierto.2.a$arm == 0, "control", "experimental")

result.mc <- nphsim::combo.wlr(survival = IPD.ascierto.2.a$time, 
                               cnsr = IPD.ascierto.2.a$event, 
                               trt = IPD.ascierto.2.a$arm, 
                               fparam = list(rgs=rgs,draws=draws))

result.mc <- lapply(result.mc, function(x) round(x, digits = 3))

as.data.frame(result.mc) %>% kable()

# rho	gamma	Zmax	pval	hr	   hrL	 hrU	  hrL.bc	hrU.bc
# 1	  1	    2.45	0	    0.798	 0.666 0.956	0.668	  0.903

########################################################################
############################### RMST ###################################
########################################################################

# nphsim package
IPD.ascierto.2.a$arm <- ifelse(IPD.ascierto.2.a$arm == 0, "control", "experimental")

IPD.ascierto.2.a$event <- ifelse(IPD.ascierto.2.a$event == 0, 1, 0)

result.rmst <- rmst.Stat(survival = IPD.ascierto.2.a$time, 
                         cnsr = IPD.ascierto.2.a$event, 
                         trt = IPD.ascierto.2.a$arm, 
                         stra = NULL, 
                         fparam = 45)

result.rmst <- lapply(result.rmst, function(x) round(x, digits = 4))

as.data.frame(result.rmst) %>% kable()

# pval	  tau	est	    estlb	  estub
# 0.01989	45	2.59362	0.12129	5.06595

# rmst2 package
result.rmst2 = rmst2(time = IPD.ascierto.2.a$time, 
                     status = IPD.ascierto.2.a$event, 
                     arm = IPD.ascierto.2.a$arm, 
                     tau = 45)

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


