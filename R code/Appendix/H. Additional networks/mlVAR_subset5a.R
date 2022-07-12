#####################################################################################
# Estimate mlVAR 
# A+M network subset 5a 
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
# moodafs_new2 <- read.csv("c:\\yourname\\moodafs_new2.csv")

#####################################################################################
# 2. Check stationarity 
#####################################################################################
stationaritycheck1_5a <- stationarity_check(moodafs_new2)
stationaritycheck2_5a <- stationarity_check2(moodafs_new2)

#####################################################################################
# 3. Apply Kalman filter 
#####################################################################################
# Not sure if function works for imputed data - does not 
# Could split data again per family role and then combine it again 
variables <- c("Family", "HAPPY.x", "SAD.x", "RELAX.x", "IRRI.x", 
               "HAPPY.y", "SAD.y", "RELAX.y", "IRRI.y")

mood_afs_new2_imp <- ema_imp(moodafs_new2, 
                             moodafs_new2$Family, 
                            variables = variables)
nrow(mood_afs_new2_imp)  # check, same number of rows 

# Plot them, only does HAPPY, SAD, RELAX, IRRI - need to be fixed  
moodx_imp5a_plot <- ema_impx_plot(mood_afs_new2_imp, mood_afs_new2_imp$Family, "moodam5a_impx_plot.pdf")
moody_imp5a_plot <- ema_impyy_plot(mood_afs_new2_imp, mood_afs_new2_imp$Family, "moodam5a_impy_plot.pdf")
# Family 1036 last four observations should be removed 
#####################################################################################
# Extra check stationarity after imputation - no significant difference 
#####################################################################################

stationaritycheck1_5a_imp <- stationarityimp_check(mood_afs_new2_imp)
stationaritycheck2_5a_imp <- stationarityimp_check2(mood_afs_new2_imp)

#####################################################################################
# 4. Estimate networks 
#####################################################################################
moodafs5a_model <- mlVAR(mood_afs_new2_imp,                                              # data set 
                        vars = c("HAPPY.x_i", "SAD.x_i", "RELAX.x_i", "IRRI.x_i",  # adolescents 
                                 "HAPPY.y_i", "SAD.y_i", "RELAX.y_i", "IRRI.y_i"),  # mothers         # fathers 
                        idvar = "Family", 
                        lags = 1, 
                        dayvar = "Day2", 
                        beepvar = "obs2", 
                        estimator = "lmer", 
                        contemporaneous = "orthogonal", 
                        temporal = "orthogonal") 
# No warnings 
# save as rds 
saveRDS(moodafs5a_model, "afsnetwork_subset5a_07072022.rds")

#####################################################################################
# 5. Plot networks 
#####################################################################################

# Plot 
Groups <- c(rep("Adolescent",4),rep("Mother",4),rep("Father",4))
temporal_show_5a <- plot(moodafs5a_model, 
                        type = "temporal",
                        lag = 1, 
                        layout = "groups",
                        edge.labels = FALSE,
                        edge.label.cex = 1,
                        repulsion = 0.4,
                        groups = Groups[c(1:4, 9:12)], 
                        theme = "colorblind",
                        color = c("#E69F00","#56B4E9"),
                        legend.cex = 0.7,
                        vsize = 9,
                        label.cex = 2,
                        esize = 23,
                        cut = 0, 
                        mar = rep(4,4), 
                        edge.label.bg = TRUE, 
                        legend = FALSE)

qgraph(temporal_show5a, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 2)))

# Without edge labels 
temporal_show_5a <- plot(moodafs5a_model, 
                         type = "temporal",
                         lag = 1, 
                         layout = "groups",
                         edge.labels = FALSE,
                         edge.label.cex = 1,
                         repulsion = 0.4,
                         groups = Groups[c(1:4, 9:12)], 
                         theme = "colorblind",
                         color = c("#E69F00","#56B4E9"),
                         legend.cex = 0.7,
                         vsize = 9,
                         label.cex = 2,
                         esize = 23,
                         cut = 0, 
                         mar = rep(4,4))

qgraph(temporal_show_5a, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 2)))

# without diagonal 
Beta_subset5a <- moodafs5a_model$results$Beta$mean
Beta_subset5a <- getNet(moodafs5a_model, "temporal", nonsig = "hide")
diag(Beta_subset5a) <- 0

temporal_show_5a2 <- qgraph(Beta_subset5a, 
                            #type = "temporal",
                            lag = 1, 
                            layout = "groups",
                            edge.labels = FALSE,
                            edge.label.cex = 1,
                            repulsion = 0.6,
                            groups = Groups[c(1:4, 9:12)], 
                            theme = "colorblind",
                            color = c("#E69F00","#56B4E9"),
                            legend.cex = 0.7,
                            vsize = 9,
                            label.cex = 2,
                            esize = 23,
                            cut = 0, 
                            mar = rep(4,4))

qgraph(temporal_show_5a2, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))


contemp_show_5a <- plot(moodafs5a_model, 
                       type = "contemporaneous",
                       lag = 1, 
                       layout = "groups",
                       edge.labels = FALSE,
                       edge.label.cex = 1.3,  # 1.5 
                       repulsion = 0.6,
                       groups = Groups[c(1:4, 9:12)], 
                       theme = "colorblind",
                       color = c("#E69F00","#56B4E9"),
                       legend.cex = 0.7,
                       vsize = 9,
                       label.cex = 2,
                       esize = 23,
                       cut = 0, 
                       legend = FALSE)

qgraph(contemp_show5a, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 2)))

layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(0.9, 1))
qgraph(contemp_show_5a, labels = c("Happy", "Sad", "Relaxed", "Irritated"), title = "Sample 5A: Contemporaneous")
qgraph(temporal_show_5a, labels = c("Happy", "Sad", "Relaxed", "Irritated"), title = "                        Temporal")
# add margins 

#####################################################################################
# Categorical 
#####################################################################################
# 3. Kalman imputation 
mood_afs5a_new2_imp_cat <- ema_imp_cat(moodafs_new2, 
                                      moodafs_new2$Family, 
                                      variables = variables)

# Extra check stationarity after imputation - no significant difference 
stationaritycheck1_5a_imp_cat <- stationarityimp_check(mood_afs5a_new2_imp_cat)
stationaritycheck2_5a_imp_cat <- stationarityimp_check2(mood_afs5a_new2_imp_cat)

# 4. Estimate networks 
moodafs5a_model_cat <- mlVAR(mood_afs5a_new2_imp_cat,                                              # data set 
                            vars = c("HAPPY.x_i", "SAD.x_i", "RELAX.x_i", "IRRI.x_i",  # adolescents 
                                     "HAPPY.y_i", "SAD.y_i", "RELAX.y_i", "IRRI.y_i"),  # mothers
                            idvar = "Family", 
                            lags = 1, 
                            dayvar = "Day2", 
                            beepvar = "obs2", 
                            estimator = "lmer", 
                            contemporaneous = "orthogonal", 
                            temporal = "orthogonal") 

# 5. Plot networks 
temporal_show5a_cat <- plot(moodafs5a_model_cat, 
                            type = "temporal",
                            lag = 1, 
                            layout = "groups",
                            edge.labels = TRUE,
                            edge.label.cex = 1,
                            repulsion = 0.4,
                            groups = Groups[1:8], 
                            theme = "colorblind",
                            color = c("#E69F00","#56B4E9"),
                            legend.cex = 0.7,
                            vsize = 9,
                            label.cex = 2,
                            esize = 23,
                            cut = 0, 
                            mar = rep(4,4))

qgraph(temporal_show5a_cat, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 2)))

# Without autoregressive effects 
# without diagonal 
Beta_subset5a_cat <- moodafs5a_model_cat$results$Beta$mean
Beta_subset5a_cat <- getNet(moodafs5a_model_cat, "temporal", nonsig = "hide")
diag(Beta_subset5a_cat) <- 0

temporal_show_5a2_cat <- qgraph(Beta_subset5c_cat, 
                                #type = "temporal",
                                lag = 1, 
                                layout = "groups",
                                edge.labels = FALSE,
                                edge.label.cex = 1,
                                repulsion = 0.4,
                                groups = Groups[c(1:4, 9:12)], 
                                theme = "colorblind",
                                color = c("#E69F00","#56B4E9"),
                                legend.cex = 0.7,
                                vsize = 9,
                                label.cex = 2,
                                esize = 23,
                                cut = 0, 
                                mar = rep(4,4))

qgraph(temporal_show_5a2_cat, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 2)))

contemp_show5a_cat <- plot(moodafs5a_model_cat, 
                           type = "contemporaneous",
                           lag = 1, 
                           layout = "groups",
                           edge.labels = TRUE,
                           edge.label.cex = 1.3,  # 1.5 
                           repulsion = 0.6,
                           groups = Groups[c(1:4, 9:12)], 
                           theme = "colorblind",
                           color = c("#E69F00","#56B4E9"),
                           legend.cex = 0.7,
                           legend.cex = 0.7,
                           vsize = 9,
                           label.cex = 2,
                           esize = 23,
                           cut = 0)

qgraph(contemp_show5a_cat, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 2)))



