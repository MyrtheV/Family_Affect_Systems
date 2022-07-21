# Figure 1: nomothetic networks 

# Load packages 
library(mlVAR)
library(qgraph)
library(ggplot2)
library(ggbeeswarm)
library(gridExtra)
library(gridBase)
library(grid)
library(cowplot)
library(gridGraphics)

# Load `famvargeneff` function from R code>Functions 

# Load model fit - adjust 'myPath' to your directory 
moodamfs_model_22102021 <- readRDS("myPath/R objects/Main/moodamfs_model_22102021.rds")


# Nomothetic networks 
## Temporal 
Groups <- c(rep("Adolescent",4),rep("Mother",4),rep("Father",4))

temporal_show_7a_22102021 <- plot(moodamfs_model_22102021, 
                                  type = "temporal",
                                  lag = 1, 
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
                                  asize = 5, 
                                  legend = FALSE)

qgraph(temporal_show_7a_22102021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), 
       edge.labels = 1:27, 
       edge.label.cex = 2, edge.label.margin = -0.025, 
       edge.label.position = 0.8, edge.label.color = "black", 
       edge.label.bg = FALSE)

## Contemporaneous 
contemp_show_20102021 <- plot(moodamfs_model_20202021, 
                              type = "contemporaneous",
                              lag = 1, 
                              layout = "groups",
                              edge.labels = TRUE,
                              edge.label.cex = 1.3,  # 1.5 
                              repulsion = 0.75,
                              groups = Groups, 
                              theme = "colorblind",
                              legend.cex = 0.7,
                              legend.cex = 0.7,
                              vsize = 9,
                              label.cex = 2,
                              esize = 23,
                              cut = 0, 
                              rule = "and", 
                              asize = 5)


qgraph(contemp_show_20102021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), 
       edge.labels = c(1, 2, 4, 3, 5, 6, 7, 8, 10, 9, 11, 12, 13, 15, 14, 16, 18, 17, 19, 20), 
       edge.label.cex = 2, edge.label.margin = -0.025, 
       edge.label.position = 0.2, edge.label.color = "black", 
       edge.label.bg = FALSE)

# Family variation 
## Use famvargeneff function 
famvargeneff_22102021 <- famvargeneff(moodamfs_model_22102021)

## Temporal 
tempfamgeneffect_22102021 <- ggplot(famvargeneff_22102021$tempfameff, mapping = aes(x = as.factor(edge), y = weight, color = edgenames)) +
  geom_quasirandom(alpha = 0.2, width = 0.2, dodge.width=1) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  geom_boxplot(famvargeneff_22102021$tempfameff, mapping = aes(x = as.factor(edge), y = weight, color = edgenames, fill = edgenames), alpha = .1) +
  geom_point(famvargeneff_22102021$tempgeneff, mapping = aes(x = as.factor(edge), y = weight), color = "black") +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Edge", y = "Weight", subtitle = "B") + 
  ylim(-0.75, 1.2)

## Contemporaenous 
contempfamgeneffect_22102021 <- ggplot(famvargeneff_22102021$confameff, mapping = aes(x = as.factor(edge), y = weight, color = edgenames)) +
  geom_quasirandom(alpha = 0.2, width = 0.2, dodge.width=1) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  geom_boxplot(famvargeneff_22102021$confameff, mapping = aes(x = as.factor(edge), y = weight, color = edgenames, fill = edgenames), alpha = .1) +
  geom_point(famvargeneff_22102021$congeneff, mapping = aes(x = as.factor(edge), y = weight), color = "black") +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Edge", y = "Weight", subtitle = "D") + 
  ylim(-0.75, 1.2)


## Temporal 
famvargeneff_22102021$tempfameff$colors <- rep(c(rep("#E69F00", 3), 
                                                 "grey", 
                                                 "#E69F00",
                                                 "grey", 
                                                 "grey",
                                                 rep("#56B4E9", 8), 
                                                 rep("grey", 4), 
                                                 rep("#009E73", 3), 
                                                 "grey", 
                                                 rep("#009E73", 2), 
                                                 "grey", 
                                                 "#009E73"), 60)


tempfamgeneffect_22102021_v2 <- ggplot(famvargeneff_22102021$tempfameff, 
                                       mapping = aes(x = as.factor(edge), 
                                                     y = weight, 
                                                     color = colors)) +
  geom_quasirandom(alpha = 0.2, width = 0.2, dodge.width=1) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  geom_boxplot(famvargeneff_22102021$tempfameff, mapping = aes(x = as.factor(edge), y = weight, color = colors, fill = colors), alpha = .1) +
  geom_point(famvargeneff_22102021$tempgeneff, mapping = aes(x = as.factor(edge), y = weight), color = "black") +
  theme_classic() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 18), 
        legend.position = "none") + 
  labs(x = "Edge", y = "Weight", subtitle = "D") + 
  ylim(-0.75, 1.2) + 
  scale_color_manual(values = c("#56B4E9",
                                "#009E73",
                                "#E69F00", 
                                "grey"), 
                     labels = c("Father", 
                                "Mother", 
                                "Adolescent", 
                                "Inter")) + 
  scale_fill_manual(values = c("#56B4E9",
                               "#009E73",
                               "#E69F00", 
                               "grey"), 
                    labels = c("Father", 
                               "Mother", 
                               "Adolescent", 
                               "Inter"))

## Contemporaneous 
famvargeneff_22102021$confameff$colors <- rep(c(rep("#E69F00", 6), 
                                                rep("#56B4E9", 6), 
                                                rep("grey", 2), 
                                                rep("#009E73", 6)), 60)

contempfamgeneffect_22102021_v2 <- ggplot(famvargeneff_22102021$confameff, mapping = aes(x = as.factor(edge), y = weight, color = colors)) +
  geom_quasirandom(alpha = 0.2, width = 0.2, dodge.width=1) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  geom_boxplot(famvargeneff_22102021$confameff, mapping = aes(x = as.factor(edge), y = weight, color = colors, fill = colors), alpha = .1) +
  geom_point(famvargeneff_22102021$congeneff, mapping = aes(x = as.factor(edge), y = weight), color = "black") +
  theme_classic() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 18)) +
  labs(x = "Edge", y = "Weight", subtitle = "B") + 
  ylim(-0.75, 1.2) + 
  scale_color_manual(values = c("#56B4E9",
                                "#009E73",
                                "#E69F00", 
                                "grey"), 
                     labels = c("Father", 
                                "Mother", 
                                "Adolescent", 
                                "Inter")) + 
  scale_fill_manual(values = c("#56B4E9",
                               "#009E73",
                               "#E69F00", 
                               "grey"), 
                    labels = c("Father", 
                               "Mother", 
                               "Adolescent", 
                               "Inter")) 



### Save legend 
legend_compfameff <- get_legend(contempfamgeneffect_22102021_v2)

## Remove legend 
contempfamgeneffect_22102021_v2 <- contempfamgeneffect_22102021_v2 + theme(legend.position = "none")


# Plot together 
par(mar = c(2.5, 2.5, 1, 1))
layout(matrix(c(1, 2, 3, 4, 1, 5, 3, 6), ncol = 2), 
       heights = c(1, 5, 1, 5), 
       widths = c(1.5, 2, 1.5, 2))
plot.new()
text(0, 0.5, "Temporal", cex = 2, font = 1)
qgraph(temporal_show_7a_22102021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), 
       edge.labels = 1:27, 
       edge.label.cex = 2, edge.label.margin = -0.025, 
       edge.label.position = 0.8, edge.label.color = "black", 
       edge.label.bg = FALSE, 
       title = "C", 
       title.cex = 1.5, 
       legend = FALSE #, 
       # mar = c(3, 3, 3, 1),
       # GLratio = 2
)

plot.new()
text(0.04, 0.5, "Contemporaneous", cex = 2, font = 1)


qgraph(contemp_show_20102021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), 
       edge.labels = c(1, 2, 4, 3, 5, 6, 7, 8, 10, 9, 11, 12, 13, 15, 14, 16, 18, 17, 19, 20), 
       edge.label.cex = 2, edge.label.margin = -0.025, 
       edge.label.position = 0.2, edge.label.color = "black", 
       edge.label.bg = FALSE, 
       title = "A", 
       title.cex = 1.5, 
       legend = FALSE)

plot.new()
vps <- baseViewports()
pushViewport(vps$figure)
vp.TopRight <- viewport(height=unit(1, "npc"), width=unit(1, "npc"), 
                        just=c("left","top"), 
                        y = 1, x = 0)
print(tempfamgeneffect_22102021_v2, vp = vp.TopRight) 
vp.BottomRight <- viewport(height=unit(1, "npc"), width=unit(1, "npc"), 
                           just=c("left","top"), 
                           y=-0.2, x=0)

print(contempfamgeneffect_22102021_v2, vp = vp.BottomRight)


## Export as .pdf with 10:13 inch range 

dev.off()

