library(ggplot2)
library(scales)
# show_col(hue_pal()(5))
library(gridExtra)
library(gridExtra)
library(tidyverse)
library(grid)

rm(list = ls())

#Scenarios considered:

#---Delayed effects

tau = 4
medianE1 = 6
medianE2 = 9
medianC = 6
time = seq(0,by=0.001,25)
hr_delayed_effects = numeric()
for(i in 1:length(time)){
  hr_delayed_effects[i] = ifelse(time[i] <= tau, medianC/medianE1, medianC/medianE2)
}

#---Crossing hazards

tau = 4
medianE1 = 9
medianE2 = 4
medianC = 6
time = seq(0,by=0.001,25)
hr_crossing = numeric()
for(i in 1:length(time)){
  hr_crossing[i] = ifelse(time[i] <= tau, medianC/medianE1, medianC/medianE2)
}

plot(time,hr_crossing,type="l")

#---Decreasing effects

tau = 4
medianC1 = 6
medianC2 = 9
medianE = 9
time = seq(0,by=0.001,25)
hr_decreasing = numeric()
for(i in 1:length(time)){
  hr_decreasing[i] = ifelse(time[i] <= tau, medianC1/medianE, medianC2/medianE)
}


df_decreasing = data.frame(time = time, hr = hr_decreasing)

df_delayed = data.frame(time = time, hr = hr_delayed_effects)

df_crossing = data.frame(time = time, hr = hr_crossing)

#Plots

p1 <- ggplot(aes(x = time, y = hr), data = df_crossing) +
  geom_hline(yintercept=1, color = "blue", linetype = "dashed", size = 1) +
  geom_line(size = 1) +
  scale_x_continuous("Time (months)") +
  ggtitle("Crossing Hazards") +
  labs(y = "Hazard ratio", color = "") +
  scale_color_manual(values=c("black", "black")) +
  theme_survminer() +
  theme(text = element_text(size = 14), legend.position = "none", plot.title = element_text(hjust = 0.5, size = 14)) +
  ylim(0.5,2)
p1

p2 <- ggplot(aes(x = time, y = hr), data = df_decreasing) +
  geom_hline(yintercept=1, color = "blue", linetype = "dashed", size = 1) +
  geom_line(size = 1) +
  scale_x_continuous("Time (months)") +
  labs(y = "Hazard ratio", color = "") +
  ggtitle("Diminishing Treatment Effect") + 
  scale_color_manual(values=c("black", "black")) +
  theme_survminer() +
  theme(text = element_text(size = 14), legend.position = "none", plot.title = element_text(hjust = 0.5, size = 14)) +
  ylim(0.6,1.1)
p2

p3 <- ggplot(aes(x = time, y = hr), data = df_delayed) +
  geom_hline(yintercept=1, color = "blue", linetype = "dashed", size = 1) +
  ylim(0.6,1.1) +
  geom_line(size = 1) +
  scale_x_continuous("Time (months)") +
  labs(y = "Hazard ratio", color = "") +
  ggtitle("Delayed Treatment Effect") + 
  scale_color_manual(values=c("black", "black")) +
  theme_survminer() +
  theme(text = element_text(size = 14), legend.position = "none", plot.title = element_text(hjust = 0.5, size = 14))
p3


scenarios = grid.arrange(p1, p2, p3,
                         nrow = 1, ncol = 3, top = textGrob("Types of NPH from Simulated Data",gp=gpar(fontsize=20,font=3)))
class(scenarios) <- c("scenarios", class(scenarios))

ggsave("~/R/MA50262 Dissertation/MA50262 Code/Simulated Data-sets/Sim_NPH_types.png",scenarios,
       width = 10, height = 4, dpi = 300)
