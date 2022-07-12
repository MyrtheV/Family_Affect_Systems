# Figure networks based on categorical data 

# Load packages 
library(ggplot)
library(mlVAR)

# Load model estimation (adjust 'myPath' to your directory)
moodamfs_model_cat_11112021 <- load("myPath/R objects/Categorical/moodamfs_model_cat_11112021.rds")

# Nomothetic temporal network 
temporal_show_cat_11112021 <- plot(moodamfs_model_cat_11112021, 
                                   type = "temporal",
                                   lag = 1, 
                                   layout = "groups",
                                   edge.labels = TRUE,
                                   edge.label.cex = 1,
                                   repulsion = 0.75,
                                   groups = Groups, 
                                   theme = "colorblind",
                                   legend.cex = 0.7,
                                   vsize = 9,
                                   label.cex = 2,
                                   esize = 23,
                                   cut = 0, 
                                   mar = rep(4,4))

qgraph(temporal_show_cat_11112021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), 
       edge.labels = 1:24, 
       edge.label.cex = 2, edge.label.margin = -0.025, 
       edge.label.position = 0.8, edge.label.color = "black", 
       edge.label.bg = FALSE)

# Nomothetic contemporaneous network 
contemp_show_cat_11112021 <- plot(moodamfs_model_cat_11112021, 
                                  type = "contemporaneous",
                                  lag = 1, 
                                  layout = "groups",
                                  edge.labels = TRUE,
                                  edge.label.cex = 1.3,  # 1.5 
                                  repulsion = 0.75,
                                  groups = Groups, 
                                  theme = "colorblind",
                                  #legend.cex = 0.7,
                                  legend.cex = 0.4,
                                  vsize = 9,
                                  label.cex = 2,
                                  esize = 23,
                                  cut = 0, 
                                  mar = c(4, 4, 4, 1), 
                                  GLratio = 5, 
                                  rule = "and")

qgraph(contemp_show_cat_11112021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), 
       edge.labels = FALSE, 
       edge.label.cex = 2, edge.label.margin = -0.025, 
       edge.label.position = 0.8, edge.label.color = "black", 
       edge.label.bg = FALSE)

# Combine networks into one plot 
layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(0.9, 1))
qgraph(contemp_show_cat_11112021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), 
       edge.labels = FALSE, 
       edge.label.cex = 2, edge.label.margin = -0.025, 
       edge.label.position = 0.8, edge.label.color = "black", 
       edge.label.bg = FALSE, 
       title = "Contemporaneous", 
       legend = FALSE, 
       mar = c(4, 4, 4, 4))
qgraph(temporal_show_cat_11112021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), 
       edge.labels = FALSE, 
       edge.label.cex = 2, edge.label.margin = -0.025, 
       edge.label.position = 0.8, edge.label.color = "black", 
       edge.label.bg = FALSE, 
       legend = FALSE, 
       title = "Temporal", 
       mar = c(4, 4, 4, 4))
dev.off()

# Note that this figure is slightly different than the figure presented in the manuscript. For the figure in the manuscript manual figure editing was used in a different software (for instance ADOBE). 

