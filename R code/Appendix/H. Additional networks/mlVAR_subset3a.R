#####################################################################################
# Estimate mlVAR 
# F (father) network subset 3a 
#####################################################################################
# Load packages/functions 
library(mlVAR)
library(ggplot2)
library(qgraph)
library(ggpubr)
library(gridExtra)
library("imputeTS")  # kalman filter 
source("c::\\yourpathhere\\stationaritycheck_functions.R")  # including functions stationarity_check(), stationarity_check(), stationarityimp_check(), and stationarityimp_check2() 
source("c::\\yourpathhere\\kalmanfilter_functions.R")  # including functions ema_imp(), mooda_imp_plot(), mooda_impx_plot(), and mooda_impyy_plot()  

#####################################################################################
# 1. Load data subset 
# (preprocessing already done, e.g., excluded participants with < 60% responses)
#####################################################################################
# father_sample3a <- read.csv("c:\\yourname\\subdata24.csv")

#####################################################################################
# 2. Check stationarity 
#####################################################################################
stationaritycheck1_3a <- stationarity_check(father_sample3a)
stationaritycheck2_3a <- stationarity_check2(father_sample3a)

#####################################################################################
# 3. Apply Kalman filter 
#####################################################################################
# Not sure if function works for imputed data - does not 
# Could split data again per family role and then combine it again 
variables <- c("HAPPY", "SAD", "RELAX", "IRRI")

father_sample3a_imp <- ema_imp(father_sample3a, 
                               father_sample3a$PPN, 
                               variables = variables)
nrow(father_sample3a_imp)  # check, same number of rows 

# Plot them, only does HAPPY, SAD, RELAX, IRRI  
father3a_imp_plot <- ema_imp_plot(father_sample3a_imp, father_sample3a_imp$Family, "father3a_imp_plot.pdf")  
dev.off()
#####################################################################################
# Extra check stationarity after imputation - no significant difference 
#####################################################################################

stationaritycheck1_3a_imp <- stationarityimp_check(father_sample3a_imp)
stationaritycheck2_3a_imp <- stationarityimp_check2(father_sample3a_imp)

#####################################################################################
# 4. Estimate networks 
#####################################################################################
father_sample3a_model <- mlVAR(father_sample3a_imp,                                              # data set 
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
saveRDS(father_sample3a_model, "fnetwork_subset3a_07072022.rds")

#####################################################################################
# 5. Plot networks 
#####################################################################################

# Plot 
Groups <- c(rep("Adolescent",4),rep("Mother",4),rep("Father",4))

# colors used in family plot 
# [1] "#E69F00" "#E69F00" "#E69F00" "#E69F00" "#009E73"
# [6] "#009E73" "#009E73" "#009E73" "#56B4E9" "#56B4E9"
# [11] "#56B4E9" "#56B4E9"

temporal_show_3a <- plot(father_sample3a_model, 
                         type = "temporal",
                         lag = 1, 
                         layout = "groups",
                         edge.labels = FALSE,
                         edge.label.cex = 1.5,
                         repulsion = 0.75,
                         groups = Groups[5:8], 
                         theme = "colorblind",
                         color = "#56B4E9",
                         legend = FALSE,
                         vsize = 9,
                         label.cex = 2,
                         esize = 23,
                         cut = 0, 
                         mar = rep(4,4))

qgraph(temporal_show_3a, labels = c("Happy", "Sad", "Relaxed", "Irritated"))

contemp_show_3a <- plot(father_sample3a_model, 
                        type = "contemporaneous",
                        lag = 1, 
                        layout = "groups",
                        edge.labels = FALSE,
                        edge.label.cex = 1.5,  # 1.5 
                        repulsion = 0.75,
                        groups = Groups[5:8], 
                        theme = "colorblind",
                        color = "#56B4E9", 
                        legend = FALSE,
                        vsize = 9,
                        label.cex = 2,
                        esize = 23,
                        cut = 0)

qgraph(contemp_show_3a, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))

layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(0.9, 1))
qgraph(contemp_show_3a, labels = c("Happy", "Sad", "Relaxed", "Irritated"), title = "Sample 3A: Contemporaneous")
qgraph(temporal_show_3a, labels = c("Happy", "Sad", "Relaxed", "Irritated"), title = "                        Temporal")
# add margins 

#####################################################################################
# Categorical 
#####################################################################################
# 3. Kalman imputation 
father_sample3a_imp_cat <- ema_imp_cat(father_sample3a, 
                                       father_sample3a$PPN, 
                                       variables = variables)

# Extra check stationarity after imputation - no significant difference 
stationaritycheck1_3a_imp_cat <- stationarityimp_check(father_sample3a_imp_cat)
stationaritycheck2_3a_imp_cat <- stationarityimp_check2(father_sample3a_imp_cat)


# 4. Estimate networks 
father_sample3a_cat_model <- mlVAR(father_sample3a_imp_cat,                                              # data set 
                                   vars = c("HAPPY_i", "SAD_i", "RELAX_i", "IRRI_i"),         # fathers 
                                   idvar = "Family", 
                                   lags = 1, 
                                   dayvar = "Day2", 
                                   beepvar = "obs2", 
                                   estimator = "lmer", 
                                   contemporaneous = "orthogonal", 
                                   temporal = "orthogonal") 

# 5. Plot networks 
temporal_show_3a_cat <- plot(father_sample3a_cat_model, 
                             type = "temporal",
                             lag = 1, 
                             layout = "groups",
                             edge.labels = TRUE,
                             edge.label.cex = 1.5,
                             repulsion = 0.75,
                             groups = Groups[5:8], 
                             theme = "colorblind",
                             color = "#56B4E9", 
                             legend = FALSE,
                             vsize = 9,
                             label.cex = 2,
                             esize = 23,
                             cut = 0, 
                             mar = rep(4,4))

qgraph(temporal_show_3a_cat, labels = c("Happy", "Sad", "Relaxed", "Irritated"))

contemp_show_3a_cat <- plot(father_sample3a_cat_model, 
                            type = "contemporaneous",
                            lag = 1, 
                            layout = "groups",
                            edge.labels = TRUE,
                            edge.label.cex = 1.5,  # 1.5 
                            repulsion = 0.75,
                            groups = Groups[5:8], 
                            theme = "colorblind",
                            color = "#56B4E9", 
                            legend = FALSE,
                            vsize = 9,
                            label.cex = 2,
                            esize = 23,
                            cut = 0)

qgraph(contemp_show_3a_cat, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))

# Slighlty different values 

