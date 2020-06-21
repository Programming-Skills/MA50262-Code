km_trt_fit <- survfit(Surv(IPD.Ohtsu.time, IPD.Ohtsu.event) ~ IPD.Ohtsu.arm)

ggsurv <- ggsurvplot(
  km_trt_fit,                     # survfit object with calculated statistics.
  data = IPD.Ohtsu,             # data used to fit survival curves.
  risk.table = TRUE,       # show risk table.
  pval = FALSE,             # show p-value of log-rank test.
  conf.int = FALSE,         # show confidence intervals for 
  # point estimates of survival curves.
  palette = c("#E7B800", "#2E9FDF"),
  xlim = c(0,23),         # present narrower X axis, but not affect
  # survival estimates.
  xlab = "Months since Randomization",   # customize X axis label.
  ylab = "Survival (probability)",   # customize X axis label.
  break.time.by = 3,     # break X axis in time intervals by 500.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.height = 0.25, # the height of the risk table
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = FALSE,      # plot the number of censored subjects at time t
  ncensor.plot.height = 0.25,
  conf.int.style = "step",  # customize style of confidence intervals
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = c("bevacizumab", "Placebo")    # change legend labels.
)

# Labels for Survival Curves (plot)
ggsurv$plot <- ggsurv$plot + labs(
  title    = "Kaplan-Meier estimates of overall survival",                  
  subtitle = "Patients in the intention-to-treat population"
)

# Changing the font size, style and color

ggsurv <- ggpar(
  ggsurv,
  font.title    = c(16, "bold", "black"),         
  font.subtitle = c(10, "bold.italic", "black"), 
  font.caption  = c(14, "plain", "black"),        
  font.x        = c(14, "bold.italic", "black"),          
  font.y        = c(14, "bold.italic", "black"),      
  font.xtickslab = c(12, "plain", "black"),
  legend = "top"
)

ggsurv