# Figure 2: family comparison 

# Load packages 
library(mlVAR)
library(qgraph)
library(ggplot2)

# Load model fit - adjust 'myPath' to your directory 
moodamfs_model_22102021 <- readRDS("myPath/R objects/Main/moodamfs_model_22102021.rds")

# Idiographic networks 
# Temporal network family A and B 
## Family A 
beta_famA <- t(moodamfs_model_22102021$results$Beta$subject[[19]][,,1])  # Beta contains the temporal relations 
length(which(abs(beta_fam19) > 0.05 ))  # 41 relations that meet the criteria of being stronger than 0.05 

temporal_show_indA <- qgraph(beta_famA, 
                              type = "temporal",
                              layout = "groups",
                              edge.labels = FALSE,
                              edge.label.cex = 1,
                              repulsion = 0.75,
                              groups = Groups, 
                              theme = "colorblind",
                              legend.cex = 0.7,
                              vsize = 9,
                              label.cex = 2,
                              esize = 23,
                              cut = 0, 
                              mar = rep(4,4), 
                              minimum = 0.05, 
                              maximum = 0.05)

qgraph(temporal_show_indA, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), legend = FALSE, 
       color = c("#EC9E27", "#F5DAB9","#F3652E"), layout = layout_pres2)

## Family B 
beta_famB <- t(moodamfs_model_22102021$results$Beta$subject[[46]][,,1])  # Beta contains the temporal relations 
length(which(abs(beta_famB) > 0.05 ))  # 43 relations that meet the criteria of being stronger than 0.05 

temporal_show_indB <- qgraph(beta_famB, 
                              type = "temporal",
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
                              mar = c(4, 4, 4, 1), 
                              minimum = 0.05, 
                              maximum = 0.05)

qgraph(temporal_show_indB, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), legend = FALSE, 
       color = c("#EC9E27", "#F5DAB9","#F3652E"), layout = layout_pres2)

# Contemporaneous 
## Family A
theta_famA <- moodamfs_model_22102021$results$Theta$pcor$subject[[19]]  # theta contains the contemporaneous relations 

contemp_show_indA <- qgraph(theta_famA, 
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
                             GLratio = 5, 
                             esize = 23,
                             cut = 0, 
                             rule = "and", 
                             minimum = 0.05, 
                             maximum = 0.05)

### Plot networks family A together 
layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(0.9, 1))
temporal_show_ind1 <- qgraph(temporal_show_indA, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), legend = FALSE, title = "Temporal")
contemp_show_ind1 <- qgraph(contemp_show_indA, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), title = "Contemporaneous")
dev.off()

# Family B 
theta_famB <- moodamfs_model_22102021$results$Theta$pcor$subject[[46]]  # theta contains the contemporaneous relations 

contemp_show_indB <- qgraph(theta_famB, 
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
                             GLratio = 5, 
                             esize = 23,
                             cut = 0, 
                             rule = "and", 
                             minimum = 0.05, 
                             maximum = 0.05)

# Plot together
## Temporal networks 
layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(0.9, 1))
temporal_show_indA <- qgraph(temporal_show_indA, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), legend = FALSE, title = "A")
temporal_show_indB <- qgraph(temporal_show_indB, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), legend = TRUE, title = "B")
dev.off()

## Contemporaneous networks 
layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(0.9, 1))
contemp_show_indA <- qgraph(contemp_show_indA, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), legend = FALSE, title = "A", color = c("#EC9E27", "#F5DAB9","#F3652E"), layout = layout_pres2)
contemp_show_indB <- qgraph(contemp_show_indB, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), title = "B", color = c("#EC9E27", "#F5DAB9","#F3652E"), layout = layout_pres2)
dev.off()

## Combine all in one panel figure 
par(mar = c(2.5, 2.5, 1, 1))
layout(matrix(c(1, 2, 3, 4, 1, 5, 3, 6), ncol = 2), heights = c(1.2, 5.7, 1.2, 5.7), widths = c(0.9, 1))
plot.new()
text(0, 0.5, "Temporal", cex = 2, font = 1)
text(0.2, 0.1, "Family A", cex = 1.2, font = 1)
text(0.7, 0.1, "Family B", cex = 1.2, font = 1)
qgraph(temporal_show_indA, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), legend = FALSE, title = "C", label.cex = 1.8)
plot.new()
text(0.04, 0.5, "Contemporaneous", cex = 2, font = 1)
qgraph(contemp_show_indA, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), legend = FALSE, title = "A", label.cex = 1.8)
qgraph(temporal_show_indB, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), legend = TRUE, title = "D", label.cex = 1.8)
qgraph(contemp_show_indB, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), title = "B", label.cex = 1.7)
dev.off()

