#####################################################################################
# Estimate mlVAR 
# A (adolescent) network subset 1b 
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
# adolescent_sample1b <- read.csv("c:\\yourname\\adolescent_sample1b.csv")

#####################################################################################
# 2. Check stationarity 
#####################################################################################
stationaritycheck1_1b <- stationarity_check(adolescent_sample1b)
stationaritycheck2_1b <- stationarity_check2(adolescent_sample1b)

#####################################################################################
# 3. Apply Kalman filter 
#####################################################################################
# Not sure if function works for imputed data - does not 
# Could split data again per family role and then combine it again 
variables <- c("HAPPY", "SAD", "RELAX", "IRRI")

adolescent_sample1b_imp <- ema_imp(adolescent_sample1b, 
                                   adolescent_sample1b$PPN, 
                           variables = variables)
nrow(adolescent_sample1b_imp)  # check, same number of rows 

# Plot them, only does HAPPY, SAD, RELAX, IRRI  
mooda_imp_plot <- ema_imp_plot(adolescent_sample1b_imp, adolescent_sample1b_imp$Family, "moodamf_imp_plot.pdf")  

#####################################################################################
# Extra check stationarity after imputation - no significant difference 
#####################################################################################

stationaritycheck1_1b_imp <- stationarityimp_check(adolescent_sample1b_imp)
stationaritycheck2_1b_imp <- stationarityimp_check2(adolescent_sample1b_imp)

#####################################################################################
# 4. Estimate networks 
#####################################################################################
adolescent_sample1b_model <- mlVAR(adolescent_sample1b_imp,                                              # data set 
                        vars = c("HAPPY_i", "SAD_i", "RELAX_i", "IRRI_i"),         # fathers 
                        idvar = "Family", 
                        lags = 1, 
                        dayvar = "Day2", 
                        beepvar = "obs2", 
                        estimator = "lmer", 
                        contemporaneous = "orthogonal", 
                        temporal = "orthogonal") 
# No warnings 
# save as rds 
saveRDS(adolescent_sample1b_model, "anetwork_subset1b_07072022.rds")

#####################################################################################
# 5. Plot networks 
#####################################################################################

# Plot 
Groups <- c(rep("Adolescent",4),rep("Mother",4),rep("Father",4))
temporal_show_1b <- plot(adolescent_sample1b_model, 
                      type = "temporal",
                      lag = 1, 
                      layout = "groups",
                      edge.labels = FALSE,
                      edge.label.cex = 1.5,
                      repulsion = 0.75,
                      groups = Groups[1:4], 
                      theme = "colorblind",
                      legend = FALSE,
                      vsize = 9,
                      label.cex = 2,
                      esize = 23,
                      cut = 0, 
                      mar = rep(4,4))

qgraph(temporal_show_1b, labels = c("Happy", "Sad", "Relaxed", "Irritated"))

contemp_show_1b <- plot(adolescent_sample1b_model, 
                     type = "contemporaneous",
                     lag = 1, 
                     layout = "groups",
                     edge.labels = FALSE,
                     edge.label.cex = 1.5,  # 1.5 
                     repulsion = 0.75,
                     groups = Groups[1:4], 
                     theme = "colorblind",
                     legend = FALSE,
                     vsize = 9,
                     label.cex = 2,
                     esize = 23,
                     cut = 0)

qgraph(contemp_show_1b, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))

layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(0.9, 1))
qgraph(contemp_show_1b, labels = c("Happy", "Sad", "Relaxed", "Irritated"), title = "Sample 1B: Contemporaneous")
qgraph(temporal_show_1b, labels = c("Happy", "Sad", "Relaxed", "Irritated"), title = "                        Temporal")
# add margins 

#####################################################################################
# Categorical 
#####################################################################################
# 3. Kalman imputation 
adolescent_sample1b_imp_cat <- ema_imp_cat(adolescent_sample1b, 
                                   adolescent_sample1b$PPN, 
                                   variables = variables)

# Extra check stationarity after imputation - no significant difference 
stationaritycheck1_1b_imp_cat <- stationarityimp_check(adolescent_sample1b_imp_cat)
stationaritycheck2_1b_imp_cat <- stationarityimp_check2(adolescent_sample1b_imp_cat)


# 4. Estimate networks 
adolescent_sample1b_cat_model <- mlVAR(adolescent_sample1b_imp_cat,                                              # data set 
                                   vars = c("HAPPY_i", "SAD_i", "RELAX_i", "IRRI_i"),         # fathers 
                                   idvar = "Family", 
                                   lags = 1, 
                                   dayvar = "Day2", 
                                   beepvar = "obs2", 
                                   estimator = "lmer", 
                                   contemporaneous = "orthogonal", 
                                   temporal = "orthogonal") 

# 5. Plot networks 
temporal_show_1b_cat <- plot(adolescent_sample1b_cat_model, 
                         type = "temporal",
                         lag = 1, 
                         layout = "groups",
                         edge.labels = TRUE,
                         edge.label.cex = 1.5,
                         repulsion = 0.75,
                         groups = Groups[1:4], 
                         theme = "colorblind",
                         legend = FALSE,
                         vsize = 9,
                         label.cex = 2,
                         esize = 23,
                         cut = 0, 
                         mar = rep(4,4))

qgraph(temporal_show_1b_cat, labels = c("Happy", "Sad", "Relaxed", "Irritated"))

contemp_show_1b_cat <- plot(adolescent_sample1b_cat_model, 
                        type = "contemporaneous",
                        lag = 1, 
                        layout = "groups",
                        edge.labels = TRUE,
                        edge.label.cex = 1.5,  # 1.5 
                        repulsion = 0.75,
                        groups = Groups[1:4], 
                        theme = "colorblind",
                        legend = FALSE,
                        vsize = 9,
                        label.cex = 2,
                        esize = 23,
                        cut = 0)

qgraph(contemp_show_1b_cat, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))

# Slighlty different values 

