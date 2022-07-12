# Main analysis - network model estimation 

# Load packages/functions 
library(mlVAR)
library(ggplot2)
library(qgraph)
library(cowplot)
library(gridExtra)
library("imputeTS")  # needed for kalman filter imputation 
source("c::\\yourpathhere/Rcode/Functions/stationaritycheck_functions.R")  # including functions stationarity_check(), stationarity_check(), stationarityimp_check(), and stationarityimp_check2() 
source("c::\\yourpathhere/R code/Functions/kalmanfilter_functions.R")  # including functions ema_imp(), mooda_imp_plot(), mooda_impx_plot(), and mooda_impyy_plot()  

#####################################################################################
# 1. Load data subset - Unfortunately, we are unable to share the data 
# (preprocessing already done, e.g., excluded participants with < 60% responses)
#####################################################################################
# In case you haven't run the samples_dfn.R file 
# mood_amfs_new2 <- read.csv("c:\\yourname\\mood_amfs_new2.csv")

#####################################################################################
# 2. Check stationarity 
#####################################################################################
stationaritycheck1_7a <- stationarity_check(mood_amfs_new2)
stationaritycheck2_7a <- stationarity_check2(mood_amfs_new2)

#####################################################################################
# 3. Apply Kalman filter 
#####################################################################################
# Not sure if function works for imputed data - does not 
# Could split data again per family role and then combine it again 
variables <- c("Family", "HAPPY.x", "SAD.x", "RELAX.x", "IRRI.x", 
               "HAPPY.y", "SAD.y", "RELAX.y", "IRRI.y", 
               "HAPPY", "SAD", "RELAX", "IRRI")

mood_a_new2_imp_22102021 <- ema_imp(mood_amfs_new2, 
                                    mood_amfs_new2$Family, 
                                    variables = variables)
nrow(mood_a_new2_imp)  # check, same number of rows 

# Plot them, only does HAPPY, SAD, RELAX, IRRI  
mooda_imp_plot <- ema_imp_plot(mood_a_new2_imp, mood_a_new2_imp$Family, "moodamf_imp_plot.pdf")  
moodx_imp_plot <- ema_impx_plot(mood_a_new2_imp, mood_a_new2_imp$Family, "moodamf_impx_plot.pdf")
moody_imp_plot <- ema_impyy_plot(mood_a_new2_imp, mood_a_new2_imp$Family, "moodamf_impy_plot.pdf")

#####################################################################################
# Extra check stationarity after imputation - no significant difference 
#####################################################################################

stationaritycheck1_7a_imp <- stationarityimp_check(mood_a_new2_imp)
stationaritycheck2_7a_imp <- stationarityimp_check2(mood_a_new2_imp)

# Descriptives 
happyhistdata <- data.frame(value = c(mood_a_new2_imp_22102021$HAPPY.x, 
                                      mood_a_new2_imp_22102021$HAPPY.y, 
                                      mood_a_new2_imp_22102021$HAPPY), 
                            group = c(rep("Adolescent", length(mood_a_new2_imp_22102021$HAPPY.x)), 
                                      rep("Mother", length(mood_a_new2_imp_22102021$HAPPY.y)), 
                                      rep("Father", length(mood_a_new2_imp_22102021$HAPPY))))

happyhistdata <- data.frame(value = c(mood_a_new2_imp_22102021$HAPPY.x, 
                                      mood_a_new2_imp_22102021$HAPPY.y, 
                                      mood_a_new2_imp_22102021$HAPPY, 
                                      mood_a_new2_imp_22102021$SAD.x, 
                                      mood_a_new2_imp_22102021$SAD.y, 
                                      mood_a_new2_imp_22102021$SAD, 
                                      mood_a_new2_imp_22102021$RELAX.x, 
                                      mood_a_new2_imp_22102021$RELAX.y, 
                                      mood_a_new2_imp_22102021$RELAX, 
                                      mood_a_new2_imp_22102021$IRRI.x, 
                                      mood_a_new2_imp_22102021$IRRI.y, 
                                      mood_a_new2_imp_22102021$IRRI), 
                            group = rep(c(rep("Adolescent", length(mood_a_new2_imp_22102021$HAPPY.x)), 
                                          rep("Mother", length(mood_a_new2_imp_22102021$HAPPY.y)), 
                                          rep("Father", length(mood_a_new2_imp_22102021$HAPPY))),4), 
                            variable = c(rep("Happy", 10080), rep ("Sad", 10080), 
                                         rep("Relaxed", 10080), rep("Irritated", 10080)))

ggplot(happyhistdata, aes(x = value, fill = group)) +  
  geom_histogram(position = "identity") + 
  scale_fill_manual(values = (c("#E69F00", "#56B4E9", "#009E73"))) + 
  facet_grid(group ~ variable) + 
  xlab("Response") + 
  ylab("Count") + 
  theme(legend.position = "None") + 
  scale_x_continuous(breaks = 1:7)


# Standard deviations per person per variable 
variables <- c("HAPPY.x", "SAD.x", "RELAX.x", "IRRI.x", 
               "HAPPY.y", "SAD.y", "RELAX.y", "IRRI.y", 
               "HAPPY", "SAD", "RELAX", "IRRI")
mean_pp_pv <- function(data, individuals, variables){
  mooda_ind_var <- matrix(NA, nrow = length(unique(individuals)), ncol = length(variables))
  for(i in 1:length(unique(individuals))){
    mooda_ind <- data[individuals == unique(individuals)[i], variables]
    
    for(j in 1:length(variables)){
      mooda_ind_var[i,j] <- mean(mooda_ind[,j], na.rm = TRUE)
    }
  } 
  colnames(mooda_ind_var) <- variables 
  return(mooda_ind_var)
}
mean_pp_pv_average <- mean_pp_pv(data = mood_a_new2_imp_22102021, individuals = mood_a_new2_imp_22102021$Family, 
                                 variables = variables)
colMeans(mean_pp_pv_average)
apply(mean_pp_pv_average, 2, sd)

sd_pp_pv <- function(data, individuals, variables){
  mooda_ind_var <- matrix(NA, nrow = length(unique(individuals)), ncol = length(variables))
  for(i in 1:length(unique(individuals))){
    mooda_ind <- data[individuals == unique(individuals)[i], variables]
    
    for(j in 1:length(variables)){
      mooda_ind_var[i,j] <- sd(mooda_ind[,j], na.rm = TRUE)
    }
  } 
  colnames(mooda_ind_var) <- variables 
  return(mooda_ind_var)
}

sd_pp_pv_average <- sd_pp_pv(data = mood_a_new2_imp_22102021, individuals = mood_a_new2_imp_22102021$Family, 
                             variables = variables)
colMeans(sd_pp_pv_average)
apply(sd_pp_pv_average, 2, sd)
#####################################################################################
# 4. Estimate networks 
#####################################################################################
moodamfs_model_22102021 <- mlVAR(mood_a_new2_imp_22102021,                                              # data set 
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

# saved as rds 
saveRDS(moodamfs_model_22102021, "amfnetwork_subset7a_22102021.rds")




