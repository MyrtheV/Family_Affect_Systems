#####################################################################################
# Estimate mlVAR 
# A+M+F network subset 7b 
#####################################################################################
# Load packages/functions 
library(mlVAR)
library(ggplot2)
library(qgraph)
library("imputeTS")  # kalman filter 
source("c::\\yourpathhere\\stationaritycheck_functions.R")  # including functions stationarity_check(), stationarity_check(), stationarityimp_check(), and stationarityimp_check2() 
source("c::\\yourpathhere\\kalmanfilter_functions.R")  # including functions ema_imp(), mooda_imp_plot(), mooda_impx_plot(), and mooda_impyy_plot()  

#####################################################################################
# 1. Load data subset 
# (preprocessing already done, e.g., excluded participants with < 60% responses)
#####################################################################################
# mood_amf <- read.csv("c:\\yourname\\mood_amf.csv")

#####################################################################################
# 2. Check stationarity 
#####################################################################################
stationaritycheck1_7b <- stationarity_check(mood_amf)
stationaritycheck2_7b <- stationarity_check2(mood_amf)

#####################################################################################
# 3. Apply Kalman filter 
#####################################################################################
# Not sure if function works for imputed data - does not 
# Could split data again per family role and then combine it again 
variables <- c("Family", "HAPPY.x", "SAD.x", "RELAX.x", "IRRI.x", 
               "HAPPY.y", "SAD.y", "RELAX.y", "IRRI.y", 
               "HAPPY", "SAD", "RELAX", "IRRI")

mood_a_new2b_imp <- ema_imp(mood_amf, 
                           mood_amf$Family, 
                           variables = variables)
nrow(mood_a_new2b_imp)  # check, same number of rows 

# Plot them, only does HAPPY, SAD, RELAX, IRRI  
mooda7b_imp_plot <- ema_imp_plot(mood_a_new2b_imp, mood_a_new2b_imp$Family, "moodamf7b_imp_plot.pdf")  
moodx7b_imp_plot <- ema_impx_plot(mood_a_new2b_imp, mood_a_new2b_imp$Family, "moodamf7b_impx_plot.pdf")
moody7b_imp_plot <- ema_impyy_plot(mood_a_new2b_imp, mood_a_new2b_imp$Family, "moodamf7b_impy_plot.pdf")

#####################################################################################
# Extra check stationarity after imputation - no significant difference 
#####################################################################################

stationaritycheck1_7b_imp <- stationarityimp_check(mood_a_new2b_imp)
stationaritycheck2_7b_imp <- stationarityimp_check2(mood_a_new2b_imp)

#####################################################################################
# 4. Estimate networks 
#####################################################################################
moodamf7b_model <- mlVAR(mood_a_new2b_imp,                                              # data set 
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
# No warnings 
# save as rds 
saveRDS(moodamf7b_model, "amfnetwork_subset7b_07072022.rds")

#####################################################################################
# 5. Plot networks 
#####################################################################################

# Plot 
Groups <- c(rep("Adolescent",4),rep("Mother",4),rep("Father",4))
temporal_show_7b <- plot(moodamf7b_model, 
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
                      legend = FALSE)

qgraph(temporal_show7b, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))

# Without edge labels 
temporal_show_7b <- plot(moodamf7b_model, 
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
                         mar = rep(4,4))

qgraph(temporal_show_7b, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))

# without diagonal 
Beta_subset7b <- moodamf7b_model$results$Beta$mean
Beta_subset7b <- getNet(moodamf7b_model, "temporal", nonsig = "hide")
diag(Beta_subset7b) <- 0

temporal_show_7b2 <- qgraph(Beta_subset7b, 
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

qgraph(temporal_show_7b2, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))


contemp_show_7b <- plot(moodamf7b_model, 
                     type = "contemporaneous",
                     lag = 1, 
                     layout = "groups",
                     edge.labels = FALSE,
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
                     legend = FALSE)

qgraph(contemp_show7b, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))

layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(0.9, 1))
qgraph(contemp_show_7b, labels = c("Happy", "Sad", "Relaxed", "Irritated"), title = "Sample 7B: Contemporaneous")
qgraph(temporal_show_7b, labels = c("Happy", "Sad", "Relaxed", "Irritated"), title = "                        Temporal")
# add margins 


#####################################################################################
# Categorical 
#####################################################################################
# 3. Kalman imputation 
mood_a_new2b_imp_cat <- ema_imp_cat(mood_amf, 
                                   mood_amf$Family, 
                                   variables = variables)

# Extra check stationarity after imputation - no significant difference 
stationaritycheck1_7b_imp_cat <- stationarityimp_check(mood_a_new2b_imp_cat)
stationaritycheck2_7b_imp_cat <- stationarityimp_check2(mood_a_new2b_imp_cat)

# 4. Estimate networks 
moodamf7b_model_cat <- mlVAR(mood_a_new2b_imp_cat,                                              # data set 
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

# 5. Plot networks 
temporal_show7b_cat <- plot(moodamf7b_model_cat, 
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

qgraph(temporal_show7b_cat, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))

# Without autoregressive effects 
# without diagonal 
Beta_subset7b_cat <- moodamf7b_model_cat$results$Beta$mean
Beta_subset7b_cat <- getNet(moodamf7b_model_cat, "temporal", nonsig = "hide")
diag(Beta_subset7b_cat) <- 0

temporal_show_7b2_cat <- qgraph(Beta_subset7b_cat, 
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

qgraph(temporal_show_7b2_cat, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))

contemp_show7b_cat <- plot(moodamf7b_model_cat, 
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
                         cut = 0)

qgraph(contemp_show7b_cat, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))

