# Figure: networks simulation 

# Load packages 
library(mlVAR)
library(ggplot2)

# Load network model fit 
moodamfs_model_11022022 <- readRDS("/Users/myrtheveenman/Documents/GitHub/DynamicFamilyNetwork1_mlVAR/Simulation Study /moodamfs_model_11022022_withoutfam60.rds")
#moodamfs_model_11022022 <- readRDS("yourPath/moodamfs_model_11022022_withoutfam60.rds")

## Contemporaneous 
contemp_sim_11022021 <- plot(moodamfs_model_11022022, 
                             type = "contemporaneous",
                             lag = 1, 
                             layout = "groups",
                             edge.labels = FALSE,
                             edge.label.cex = 1.3,  # 1.5 
                             repulsion = 0.75,
                             groups = Groups, 
                             theme = "colorblind",
                             legend.cex = 0.55,
                             vsize = 9,
                             label.cex = 2,
                             esize = 23,
                             cut = 0, 
                             rule = "and", 
                             title = "Contemporaneous", 
                             legend = FALSE)

qgraph(contemp_sim_11022021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))

## Temporal 
temporal_sim_11022021 <- plot(moodamfs_model_11022022, 
                              type = "temporal",
                              lag = 1, 
                              layout = "groups",
                              edge.labels = FALSE,
                              edge.label.cex = 1.3,
                              repulsion = 0.75,
                              groups = Groups, 
                              theme = "colorblind",
                              legend.cex = 0.55,
                              vsize = 9,
                              label.cex = 2,
                              esize = 23,
                              cut = 0, 
                              mar = rep(4,4), 
                              title = "Temporal", 
                              legend = FALSE)

qgraph(temporal_sim_11022021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))


layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(0.9, 1))
qgraph(contemp_sim_11022021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))
qgraph(temporal_sim_11022021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))
dev.off()  # export as .pdf 6x11 

