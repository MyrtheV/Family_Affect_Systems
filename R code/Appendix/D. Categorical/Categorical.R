#####################################################################################
# Categorical 
#####################################################################################
# 1. Load packages 
library(mlVAR)
library(qgraph)
library(imputeTS)
library(ggplot2)

# 2. Load data 
# Unfortunately, we are unable to share the data 

# 3. Kalman imputation 
variables <- c("Family", "HAPPY.x", "SAD.x", "RELAX.x", "IRRI.x", 
               "HAPPY.y", "SAD.y", "RELAX.y", "IRRI.y", 
               "HAPPY", "SAD", "RELAX", "IRRI")
mood_a_new2_imp_cat_11112021 <- ema_imp_cat(mood_amfs_new2, 
                                            mood_amfs_new2$Family, 
                                            variables = variables)

# Extra check stationarity after imputation - no significant difference 
stationaritycheck1_7a_imp_cat <- stationarityimp_check(mood_a_new2_imp_cat)
stationaritycheck2_7a_imp_cat <- stationarityimp_check2(mood_a_new2_imp_cat)

# 4. Estimate networks 
moodamfs_model_cat_11112021 <- mlVAR(mood_a_new2_imp_cat_11112021,                                              # data set 
                                     vars = c("HAPPY.x_i", "SAD.x_i", "RELAX.x_i", "IRRI.x_i",  # adolescents 
                                              "HAPPY.y_i", "SAD.y_i", "RELAX.y_i", "IRRI.y_i",  # mothers 
                                              "HAPPY_i", "SAD_i", "RELAX_i", "IRRI_i"),         # fathers 
                                     idvar = "Family", 
                                     lags = 1, 
                                     dayvar = "Day2", 
                                     beepvar = "obs2", 
                                     estimator = "lmer", 
                                     contemporaneous = "orthogonal", 
                                     temporal = "orthogonal") 

saveRDS(moodamfs_model_cat_11112021, "moodamfs_model_cat_11112021.rds")

# 5. Plot networks 
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

# Without autoregressive effects 
# without diagonal 
Beta_subset7a_cat <- moodamfs_model_cat$results$Beta$mean
Beta_subset7a_cat <- getNet(moodamfs_model_cat, "temporal", nonsig = "hide")
diag(Beta_subset7a_cat) <- 0

temporal_show_7a2_cat <- qgraph(Beta_subset7a_cat, 
                                #type = "temporal",
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
                                mar = rep(4,4))

qgraph(temporal_show_7a2_cat, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))

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

layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(0.9, 1))
qgraph(temporal_show_cat_11112021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), 
       edge.labels = FALSE, 
       edge.label.cex = 2, edge.label.margin = -0.025, 
       edge.label.position = 0.8, edge.label.color = "black", 
       edge.label.bg = FALSE, 
       legend = FALSE, 
       title = "Temporal", 
       mar = c(4, 4, 4, 4))
qgraph(contemp_show_cat_11112021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), 
       edge.labels = FALSE, 
       edge.label.cex = 2, edge.label.margin = -0.025, 
       edge.label.position = 0.8, edge.label.color = "black", 
       edge.label.bg = FALSE, 
       title = "Contemporaneous", 
       legend = FALSE, 
       mar = c(4, 4, 4, 4))


# Comparison with networks in paper (continuous data)

## Temporal 
## With autoregressive effects 
temp_show_plot <- qgraph(temporal_show, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), title = "Continuous", legend = FALSE)
temp_show_plot_cat <- qgraph(temporal_show_cat, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), title = "Categorical", legend.cex = 0.5)

## Without autoregressive effects 
tempwithout_show_plot <- qgraph(temporal_show_7a2, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), title = "Continuous", legend = FALSE)
tempwithout_show_plot_cat <- qgraph(temporal_show_7a2_cat, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), title = "Categorical", legend.cex = 0.5)

## Contemporaneous 
contemp_show_plot <- qgraph(contemp_show, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), title = "Continuous", legend = FALSE)
contemp_show_plot_cat <- qgraph(contemp_show_cat, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)), title = "Categorical", legend.cex = 0.5)


par(mfrow = c(1,2))
plot(contemp_show_plot)
plot(contemp_show_plot_cat)
dev.off()

## Correlate adjacency matrices 
## temporal effects 
cor(moodamfs_model_22102021$results$Beta$mean, moodamfs_model_cat_11112021$results$Beta$mean)  # indeed very high: 0.9888418 
sum(abs(moodamfs_model_22102021$results$Beta$mean - moodamfs_model_cat_11112021$results$Beta$mean))

cor(c(getNet(moodamfs_model_22102021, "temporal", nonsig = "hide")), c(getNet(moodamfs_model_cat_11112021, "temporal", nonsig = "hide")))
sum(abs(c(getNet(moodamfs_model_22102021, "temporal", nonsig = "hide")) - c(getNet(moodamfs_model_cat_11112021, "temporal", nonsig = "hide"))))

### check differences 
(getNet(moodamfs_model_22102021, "temporal", nonsig = "hide")==0)==(getNet(moodamfs_model_cat_11112021, "temporal", nonsig = "hide")==0)

## contemporaneous effects 
contemp_cont_7a <- moodamfs_model_22102021$results$Theta$pcor$mean
diag(contemp_cont_7a) <- 0

contemp_cat_7a <- moodamfs_model_cat_11112021$results$Theta$pcor$mean
diag(contemp_cat_7a) <- 0

cor(c(contemp_cont_7a), c(contemp_cat_7a))  # 0.9986331   - use this for paper 
sum(abs(contemp_cont_7a - contemp_cat_7a))  # 0.9986331)

## actual shown in plot
cor(c(getNet(moodamfs_model_22102021, "contemporaneous", nonsig = "hide", rule = "and")), c(getNet(moodamfs_model_cat_11112021, "contemporaneous", nonsig = "hide", rule = "and")))
sum(abs(c(getNet(moodamfs_model_22102021, "contemporaneous", nonsig = "hide", rule = "and"))- c(getNet(moodamfs_model_cat_11112021, "contemporaneous", nonsig = "hide", rule = "and"))))


