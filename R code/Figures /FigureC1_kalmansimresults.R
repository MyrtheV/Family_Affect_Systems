# Figure Kalman simulation 

# Load packages 
library(ggplot2)

# Load results simulation - adjust 'myPath' to your directory 
Kalman_sim_long <- readRDS("myPath/R objects/Kalman simulation/Kalman_sim_res.rds")

ggplot(Kalman_sim_long, aes(x = as.factor(missing), y = value)) + 
  geom_boxplot() + 
  xlab("Missingness (%)") + 
  ylab("Correlation") + 
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey", size = .3) + 
  geom_hline(yintercept = 0, color = "grey", size = .3) + 
  guides(fill=guide_legend(title="Affect")) + 
  theme_classic() + 
  scale_y_continuous(n.breaks = 10) 

