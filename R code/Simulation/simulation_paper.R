####################################################################################################
# Simulation 
####################################################################################################

####################################################################################################
# Load packages 
####################################################################################################
library("mlVAR")
library("parSim")
library("plyr")
library("dplyr")
library("ggplot2")

####################################################################################################
# Load model for simulation 
####################################################################################################
moodamfs_model_11022022 <- readRDS("yourPath/R objects/Simulation/Network model/moodamfs_model_11022022_withoutfam60.rds")
# adjust 'yourPath' to your directory 

# Figure of networks shown in Appendix 
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

layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(0.9, 1))
qgraph(contemp_sim_11022021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))
qgraph(temporal_sim_11022021, labels = (rep(c("Happy", "Sad", "Relaxed", "Irritated"), 3)))
dev.off()  # export as .pdf 6x11 

# Simulation using mlVARsample() 
dynfamnetwork_newsim_withoutfam60_1rep_23032022 <- mlVAR::mlVARsample(moodamfs_model_11022022, 
                                                                         nTime = c(20, 56, 100), 
                                                                         nSample = c(30, 45, 59), 
                                                                         pMissing = c(0, 0.1, 0.25, 0.5), 
                                                                         nReps = 1)

####################################################################################################
# Simulation function 
####################################################################################################
## Add this to a repetition and write results to r file combining all the repetitions 
## Allow to pause the simulation - instead do it in parts 
sim_famnetwork_repfunc <- function(modelfit, rep, date){
  
  namefile <- paste("famnetwork_simres_", date, ".rds", sep = "")
  
  for(i in 1:rep){
    if(i == 1){
      # set.seed(i)
      famnetwork_simres <- mlVAR::mlVARsample(modelfit, 
                                        nTime = c(20, 56, 100), 
                                        nSample = c(30, 45, 59), 
                                        pMissing = c(0, 0.1, 0.25, 0.5), 
                                        nReps = 1)
      
      # Save results in .rds file 
      saveRDS(famnetwork_simres, namefile)
      
    } else {
      # set.seed(i)
      sim_results <- mlVAR::mlVARsample(modelfit, 
                                        nTime = c(20, 56, 100), 
                                        nSample = c(30, 45, 59), 
                                        pMissing = c(0, 0.1, 0.25, 0.5), 
                                        nReps = 1)
      
      # Combine results with earlier results 
      famnetwork_simres <- rbind(famnetwork_simres, sim_results)
      
      # Save results in .rds file 
      saveRDS(famnetwork_simres, namefile)
      
    }
  }
}

# Try function with a simpler simulation that is faster 
sim_famnetwork_repfunc_try <- function(modelfit, rep, date){
  
  namefile <- paste("famnetwork_simres_", date, ".rds", sep = "")
  
  for(i in 1:rep){
    if(i == 1){
      # set.seed(i)
      famnetwork_simres <- mlVAR::mlVARsample(modelfit, 
                                              nTime = 56, 
                                              nSample = 59, 
                                              pMissing = 0, 
                                              nReps = 1)
      
      # Save results in .rds file 
      saveRDS(famnetwork_simres, namefile)
      
    } else {
      # set.seed(i)
      sim_results <- mlVAR::mlVARsample(modelfit, 
                                        nTime = 56, 
                                        nSample = 59, 
                                        pMissing = 0, 
                                        nReps = 1)
      
      # Combine results with earlier results 
      famnetwork_simres <- rbind(famnetwork_simres, sim_results)
      
      # Save results in .rds file 
      saveRDS(famnetwork_simres, namefile)
      
    }
  }
}
sim_famnetwork_repfunc_try(moodamfs_model_11022022, 5, 23022022)
sim_famnetwork_repfunc_try1 <- readRDS("yourPath/R objects/Simulation/Results/Seperate files/famnetwork_simres_23022022.rds")

####################################################################################################
# Results simulation paper 
####################################################################################################
sim_famnetwork_repfunc(moodamfs_model_11022022, 100, 11042022)

## Load .rds files with results 
sim_famnetwork_res1 <- readRDS("/Users/myrtheveenman/famnetwork_simres_23022022.rds")
sim_famnetwork_res2 <- readRDS("/Users/myrtheveenman/famnetwork_simres_25022022.rds")
sim_famnetwork_res3 <- readRDS("/Users/myrtheveenman/famnetwork_simres_28022022.rds")
sim_famnetwork_res4 <- readRDS("/Users/myrtheveenman/famnetwork_simres_30032022.rds")
sim_famnetwork_res5 <- readRDS("/Users/myrtheveenman/famnetwork_simres_1042022.rds")
sim_famnetwork_res6 <- readRDS("/Users/myrtheveenman/famnetwork_simres_3042022.rds")
sim_famnetwork_res7 <- readRDS("/Users/myrtheveenman/famnetwork_simres_5042022.rds")
sim_famnetwork_res8 <- readRDS("/Users/myrtheveenman/famnetwork_simres_6042022.rds")
sim_famnetwork_res9 <- readRDS("/Users/myrtheveenman/famnetwork_simres_7042022.rds")
sim_famnetwork_res10 <- readRDS("/Users/myrtheveenman/famnetwork_simres_11042022.rds")

## Combine .rds files into one result file 
sim_famnetwork_res_total <- rbind(sim_famnetwork_res1, sim_famnetwork_res2, sim_famnetwork_res3, 
                                  sim_famnetwork_res4, sim_famnetwork_res5, sim_famnetwork_res6, 
                                  sim_famnetwork_res7, sim_famnetwork_res8, sim_famnetwork_res9, 
                                  sim_famnetwork_res10)

saveRDS(sim_famnetwork_res_total, "sim_famnetwork_res_total_13042022.rds")

####################################################################################################
# Figure 
####################################################################################################
# Adjust below 

####################################################################################################
## Figure - Functions needed 
####################################################################################################
# 1. Transform data structure to function with ggplot2 
## Function if using function from mlVAR package 
plotsimfunc <- function(dynfamnet_newsim1){
  # Remove errors 
  dynfamnet_newsim_res <- dynfamnet_newsim1[dynfamnet_newsim1$error == FALSE,]
  # Remove error + error messsage 
  dynfamnet_newsim_res2 <- dynfamnet_newsim_res[,!names(dynfamnet_newsim_res) %in% c("error", "errorMessage")]
  
  # Change format data 
  corsimres <- data.frame(nTime = dynfamnet_newsim_res2$nTime, nind = dynfamnet_newsim_res2$nSample, 
                          nmissing = dynfamnet_newsim_res2$pMissing, rep = dynfamnet_newsim_res2$rep, 
                          id = dynfamnet_newsim_res2$id, network = dynfamnet_newsim_res2$network, 
                          value = dynfamnet_newsim_res2$correlation, measure = rep("Correlation", nrow(dynfamnet_newsim_res2)))
  
  senssimres <- data.frame(nTime = dynfamnet_newsim_res2$nTime, nind = dynfamnet_newsim_res2$nSample, 
                           nmissing = dynfamnet_newsim_res2$pMissing, rep = dynfamnet_newsim_res2$rep, 
                           id = dynfamnet_newsim_res2$id, network = dynfamnet_newsim_res2$network, 
                           value = dynfamnet_newsim_res2$sensitivity, measure = rep("Sensitivity", nrow(dynfamnet_newsim_res2)))
  
  
  specsimres <- data.frame(nTime = dynfamnet_newsim_res2$nTime, nind = dynfamnet_newsim_res2$nSample, 
                           nmissing = dynfamnet_newsim_res2$pMissing, rep = dynfamnet_newsim_res2$rep, 
                           id = dynfamnet_newsim_res2$id, network = dynfamnet_newsim_res2$network, 
                           value = dynfamnet_newsim_res2$specificity, measure = rep("Specificity", nrow(dynfamnet_newsim_res2)))
  
  biassimres <- data.frame(nTime = dynfamnet_newsim_res2$nTime, nind = dynfamnet_newsim_res2$nSample, 
                           nmissing = dynfamnet_newsim_res2$pMissing, rep = dynfamnet_newsim_res2$rep, 
                           id = dynfamnet_newsim_res2$id, network = dynfamnet_newsim_res2$network, 
                           value = dynfamnet_newsim_res2$bias, measure = rep("Bias", nrow(dynfamnet_newsim_res2)))
  
  precsimres <- data.frame(nTime = dynfamnet_newsim_res2$nTime, nind = dynfamnet_newsim_res2$nSample, 
                           nmissing = dynfamnet_newsim_res2$pMissing, rep = dynfamnet_newsim_res2$rep, 
                           id = dynfamnet_newsim_res2$id, network = dynfamnet_newsim_res2$network, 
                           value = dynfamnet_newsim_res2$precision, measure = rep("Precision", nrow(dynfamnet_newsim_res2)))
  
  dynfamnet_newsim_res3 <- rbind(corsimres, senssimres, specsimres, biassimres, precsimres)
  return(dynfamnet_newsim_res3)
} 

# Transform results to data frame 
sim_famnetwork_res_total_13042022 <- plotsimfunc(sim_famnetwork_res_total)
write.csv(sim_famnetwork_res_total_13042022, "sim_famnetwork_res_total_13042022.csv")  # save as .csv file 


#################################################################################################
## Figure paper  
#################################################################################################
# Use networks shown in paper 
shortdatasetpermeasure_func <- function(dynfamnet_newsim_res3_3012_2021_1000rep){
  datalistpermeasure <- list()
  
  # All 
  networksincl <- c("temporal_thresholded", "contemporaneous_thresholded",  
                    "temporal_subject", "contemporaneous_subject")
  
  dynfamnet_newsim_res3_3012_2021_1000rep_short <- dynfamnet_newsim_res3_3012_2021_1000rep[dynfamnet_newsim_res3_3012_2021_1000rep$network %in% networksincl,]
  datalistpermeasure$all <-  dynfamnet_newsim_res3_3012_2021_1000rep_short 
  
  # Correlation 
  datalistpermeasure$cor <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$measure == "Correlation",]
  
  
  # Bias 
  datalistpermeasure$bias <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$measure == "Bias",]
  
  # Precision 
  datalistpermeasure$prec <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$measure == "Precision",]
  
  # Sensitivity 
  datalistpermeasure$sens <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$measure == "Sensitivity",]
  
  # Specificity 
  datalistpermeasure$spec <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$measure == "Specificity",]
  
  return(datalistpermeasure)
}

sim_famnetwork_res_total_13042022_short <- shortdatasetpermeasure_func(sim_famnetwork_res_total_13042022)

# Tables (succesful repetitions per condition)
rep_per_cor <- table(sim_famnetwork_res_total_13042022_short$cor$network, 
                     sim_famnetwork_res_total_13042022_short$cor$nmissing, 
                     sim_famnetwork_res_total_13042022_short$cor$nind, 
                     sim_famnetwork_res_total_13042022_short$cor$nTime)

rep_per_bias <- table(sim_famnetwork_res_total_13042022_short$bias$network, 
                      sim_famnetwork_res_total_13042022_short$bias$nmissing, 
                      sim_famnetwork_res_total_13042022_short$bias$nind, 
                      sim_famnetwork_res_total_13042022_short$bias$nTime)

rep_per_prec <- table(sim_famnetwork_res_total_13042022_short$prec$network, 
                      sim_famnetwork_res_total_13042022_short$prec$nmissing, 
                      sim_famnetwork_res_total_13042022_short$prec$nind, 
                      sim_famnetwork_res_total_13042022_short$prec$nTime) 

rep_per_sens <- table(sim_famnetwork_res_total_13042022_short$sens$network, 
                      sim_famnetwork_res_total_13042022_short$sens$nmissing, 
                      sim_famnetwork_res_total_13042022_short$sens$nind, 
                      sim_famnetwork_res_total_13042022_short$sens$nTime)

rep_per_spec <- table(sim_famnetwork_res_total_13042022_short$spec$network, 
                      sim_famnetwork_res_total_13042022_short$spec$nmissing, 
                      sim_famnetwork_res_total_13042022_short$spec$nind, 
                      sim_famnetwork_res_total_13042022_short$spec$nTime)


# Function to save figures 
## Saves the figures of different number of families 
## Can also run ggplots outside of function but input data has to be changed into `sim_famnetwork_res_total_13042022_short$all` instead of `dynfamnet_newsim_res3_3012_2021_1000rep_short`
simulationfigurefunc <- function(dynfamnet_newsim_res3_3012_2021_1000rep_short){
  library(gridExtra)
  
  dynfamnet_newsim_res3_3012_2021_1000rep_short1 <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$nTime == 56,]
  
  simfig_short1 <- ggplot(dynfamnet_newsim_res3_3012_2021_1000rep_short1) + 
    geom_boxplot(aes(x = as.factor(nmissing), y = value, fill = network, colour = network), alpha = 0.2) + 
    facet_grid(nind ~ measure) + 
    xlab("% missing data") + 
    ylab("Value") + 
    theme_bw() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
  #ylim(-0.25,1) 
  
  # 100 observations 
  dynfamnet_newsim_res3_3012_2021_1000rep_short2 <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$nTime == 100,]
  
  simfig_short2 <- ggplot(dynfamnet_newsim_res3_3012_2021_1000rep_short2) + 
    geom_boxplot(aes(x = as.factor(nmissing), y = value, fill = network, colour = network), alpha = 0.2) + 
    facet_grid(nind ~ measure) + 
    xlab("% missing data") + 
    ylab("Value") + 
    theme_bw() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
  
  # ylim(-0.25,1) 
  
  # Issue: No observations for 60 families... 
  
  # 20 observations 
  dynfamnet_newsim_res3_3012_2021_1000rep_short3 <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$nTime == 20,]
  
  simfig_short3 <- ggplot(dynfamnet_newsim_res3_3012_2021_1000rep_short3) + 
    geom_boxplot(aes(x = as.factor(nmissing), y = value, fill = network, colour = network), alpha = 0.2) + 
    facet_grid(nind ~ measure) + 
    xlab("% missing data") + 
    ylab("Value") + 
    theme_bw() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) + 
    guides(color=guide_legend("Network"), fill=guide_legend("Network")) + 
    scale_fill_manual(labels = c("Family contemporaneous networks", "General contemporaneous network", 
                                 "Family temporal networks", "General temporal network"), 
                      values = c("#F8766D", "#7CAE00", 
                                 "#00BFC4", "#C77CFF")) +
    scale_color_manual(labels = c("Family contemporaneous networks", "General contemporaneous network", 
                                  "Family temporal networks", "General temporal network"), 
                       values = c("#F8766D", "#7CAE00", 
                                  "#00BFC4", "#C77CFF")) 
  
  # ylim(-0.25,1) 
  
  # Number of families 
   numbertimepoints <- c(`20` = "20 time points", 
                        `56` = "56 time points", 
                        `100` = "100 time points")
  
  # 30 families 
  dynfamnet_newsim_res3_3012_2021_1000rep_fam1 <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$nind == 30,]
  
  simfig_fam1 <- ggplot(dynfamnet_newsim_res3_3012_2021_1000rep_fam1) + 
    geom_boxplot(aes(x = as.factor(nmissing), y = value, fill = network, colour = network), alpha = 0.2) + 
    facet_grid(nTime ~ measure, labeller = labeller(nTime = as_labeller(numbertimepoints))) + 
    xlab("% missing data") + 
    ylab("Value") + 
    theme_bw() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) + 
    guides(color=guide_legend("Network"), fill=guide_legend("Network")) + 
    scale_fill_manual(labels = c("Idiographic contemporaneous networks", "Nomothetic contemporaneous network", 
                                 "Idiographic temporal networks", "Nomothetic temporal network"), 
                      values = c("#F8766D", "#7CAE00", 
                                 "#00BFC4", "#C77CFF")) +
    scale_color_manual(labels = c("Idiographic contemporaneous networks", "Nomothetic contemporaneous network", 
                                  "Idiographic temporal networks", "Nomothetic temporal network"), 
                       values = c("#F8766D", "#7CAE00", 
                                  "#00BFC4", "#C77CFF")) 
  
  # ylim(-0.25,1) 
  
  # 45 families 
  dynfamnet_newsim_res3_3012_2021_1000rep_fam2 <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$nind == 45,]
  
  simfig_fam2 <- ggplot(dynfamnet_newsim_res3_3012_2021_1000rep_fam2) + 
    geom_boxplot(aes(x = as.factor(nmissing), y = value, fill = network, colour = network), alpha = 0.2) + 
    facet_grid(nTime ~ measure, labeller = labeller(nTime = as_labeller(numbertimepoints))) + 
    xlab("% missing data") + 
    ylab("Value") + 
    theme_bw() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) + 
    guides(color=guide_legend("Network"), fill=guide_legend("Network")) + 
    scale_fill_manual(labels = c("Idiographic contemporaneous networks", "Nomothetic contemporaneous network", 
                                 "Idiographic temporal networks", "Nomothetic temporal network"), 
                      values = c("#F8766D", "#7CAE00", 
                                 "#00BFC4", "#C77CFF")) +
    scale_color_manual(labels = c("Idiographic contemporaneous networks", "Nomothetic contemporaneous network", 
                                  "Idiographic temporal networks", "Nomothetic temporal network"), 
                       values = c("#F8766D", "#7CAE00", 
                                  "#00BFC4", "#C77CFF")) 
  # ylim(-0.25,1) 
  
  # 59 families 
  dynfamnet_newsim_res3_3012_2021_1000rep_fam3 <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$nind == 59,]
  
  simfig_fam3 <- ggplot(dynfamnet_newsim_res3_3012_2021_1000rep_fam3) + 
    geom_boxplot(aes(x = as.factor(nmissing), y = value, fill = network, colour = network), alpha = 0.2) + 
    facet_grid(nTime ~ measure, labeller = labeller(nTime = as_labeller(numbertimepoints))) + 
    xlab("% missing data") + 
    ylab("Value") + 
    theme_bw() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) + 
    guides(color=guide_legend("Network"), fill=guide_legend("Network")) + 
    scale_fill_manual(labels = c("Idiographic contemporaneous networks", "Nomothetic contemporaneous network", 
                                 "Idiographic temporal networks", "Nomothetic temporal network"), 
                      values = c("#F8766D", "#7CAE00", 
                                 "#00BFC4", "#C77CFF")) +
    scale_color_manual(labels = c("Idiographic contemporaneous networks", "Nomothetic contemporaneous network", 
                                  "Idiographic temporal networks", "Nomothetic temporal network"), 
                       values = c("#F8766D", "#7CAE00", 
                                  "#00BFC4", "#C77CFF")) 
  # ylim(-0.25,1) 
  
  # Number of missingness 
  # 0 % 
  dynfamnet_newsim_res3_3012_2021_1000rep_miss1 <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$nmissing == 0,]
  
  simfig_miss1 <- ggplot(dynfamnet_newsim_res3_3012_2021_1000rep_miss1) + 
    geom_boxplot(aes(x = as.factor(nind), y = value, fill = network, colour = network), alpha = 0.2) + 
    facet_grid(nTime ~ measure) + 
    xlab("Number of families") + 
    ylab("Value") + 
    theme_bw() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
  
  # 10 % 
  dynfamnet_newsim_res3_3012_2021_1000rep_miss2 <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$nmissing == 0.1,]
  
  simfig_miss2 <- ggplot(dynfamnet_newsim_res3_3012_2021_1000rep_miss2) + 
    geom_boxplot(aes(x = as.factor(nind), y = value, fill = network, colour = network), alpha = 0.2) + 
    facet_grid(nTime ~ measure) + 
    xlab("Number of families") + 
    ylab("Value") + 
    theme_bw() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
  
  # 25 % 
  dynfamnet_newsim_res3_3012_2021_1000rep_miss3 <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$nmissing == 0.25,]
  
  simfig_miss3 <- ggplot(dynfamnet_newsim_res3_3012_2021_1000rep_miss3) + 
    geom_boxplot(aes(x = as.factor(nind), y = value, fill = network, colour = network), alpha = 0.2) + 
    facet_grid(nTime ~ measure) + 
    xlab("Number of families") + 
    ylab("Value") + 
    theme_bw() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
  
  # 50 % 
  dynfamnet_newsim_res3_3012_2021_1000rep_miss4 <- dynfamnet_newsim_res3_3012_2021_1000rep_short[dynfamnet_newsim_res3_3012_2021_1000rep_short$nmissing == 0.5,]
  
  simfig_miss4 <- ggplot(dynfamnet_newsim_res3_3012_2021_1000rep_miss4) + 
    geom_boxplot(aes(x = as.factor(nind), y = value, fill = network, colour = network), alpha = 0.2) + 
    facet_grid(nTime ~ measure) + 
    xlab("Number of families") + 
    ylab("Value") + 
    theme_bw() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
  
  
  # Save figures for paper as .png 
  ggsave(file = "simfig_fam1.png", simfig_fam1, width = 12, height = 8)
  ggsave(file = "simfig_fam2.png", simfig_fam2, width = 12, height = 8)
  ggsave(file = "simfig_fam3.png", simfig_fam3, width = 12, height = 8)
  
} 

simulationfigurefunc(sim_famnetwork_res_total_13042022_short$all)  # saves the three figures per number of families as .png files in the directory you are working in 

# Or don't use the function and fit them separately 
# Number of families - labels facet_grid 
numbertimepoints <- c(`20` = "20 time points", 
                      `56` = "56 time points", 
                      `100` = "100 time points")

# 30 families 
sim_famnetwork_res_total_13042022_fam1 <- sim_famnetwork_res_total_13042022_short$all[sim_famnetwork_res_total_13042022_short$all$nind == 30,]

simfig_fam1 <- ggplot(sim_famnetwork_res_total_13042022_fam1) + 
  geom_boxplot(aes(x = as.factor(nmissing), y = value, fill = network, colour = network), alpha = 0.2) + 
  facet_grid(nTime ~ measure, labeller = labeller(nTime = as_labeller(numbertimepoints))) + 
  xlab("% missing data") + 
  ylab("Value") + 
  theme_bw() + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) + 
  guides(color=guide_legend("Network"), fill=guide_legend("Network")) + 
  scale_fill_manual(labels = c("Idiographic contemporaneous networks", "Nomothetic contemporaneous network", 
                               "Idiographic temporal networks", "Nomothetic temporal network"), 
                    values = c("#F8766D", "#7CAE00", 
                               "#00BFC4", "#C77CFF")) +
  scale_color_manual(labels = c("Idiographic contemporaneous networks", "Nomothetic contemporaneous network", 
                                "Idiographic temporal networks", "Nomothetic temporal network"), 
                     values = c("#F8766D", "#7CAE00", 
                                "#00BFC4", "#C77CFF")) 


# 45 families 
sim_famnetwork_res_total_13042022_fam2 <- sim_famnetwork_res_total_13042022_short$all[sim_famnetwork_res_total_13042022_short$all$nind == 45,]

simfig_fam2 <- ggplot(sim_famnetwork_res_total_13042022_fam2) + 
  geom_boxplot(aes(x = as.factor(nmissing), y = value, fill = network, colour = network), alpha = 0.2) + 
  facet_grid(nTime ~ measure, labeller = labeller(nTime = as_labeller(numbertimepoints))) + 
  xlab("% missing data") + 
  ylab("Value") + 
  theme_bw() + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) + 
  guides(color=guide_legend("Network"), fill=guide_legend("Network")) + 
  scale_fill_manual(labels = c("Idiographic contemporaneous networks", "Nomothetic contemporaneous network", 
                               "Idiographic temporal networks", "Nomothetic temporal network"), 
                    values = c("#F8766D", "#7CAE00", 
                               "#00BFC4", "#C77CFF")) +
  scale_color_manual(labels = c("Idiographic contemporaneous networks", "Nomothetic contemporaneous network", 
                                "Idiographic temporal networks", "Nomothetic temporal network"), 
                     values = c("#F8766D", "#7CAE00", 
                                "#00BFC4", "#C77CFF")) 

# 59 families 
sim_famnetwork_res_total_13042022_fam3 <- sim_famnetwork_res_total_13042022_short$all[sim_famnetwork_res_total_13042022_short$all$nind == 59,]

simfig_fam3 <- ggplot(sim_famnetwork_res_total_13042022_fam3) + 
  geom_boxplot(aes(x = as.factor(nmissing), y = value, fill = network, colour = network), alpha = 0.2) + 
  facet_grid(nTime ~ measure, labeller = labeller(nTime = as_labeller(numbertimepoints))) + 
  xlab("% missing data") + 
  ylab("Value") + 
  theme_bw() + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) + 
  guides(color=guide_legend("Network"), fill=guide_legend("Network")) + 
  scale_fill_manual(labels = c("Idiographic contemporaneous networks", "General contemporaneous network", 
                               "Idiographic temporal networks", "General temporal network"), 
                    values = c("#F8766D", "#7CAE00", 
                               "#00BFC4", "#C77CFF")) +
  scale_color_manual(labels = c("Idiographic contemporaneous networks", "Nomothetic contemporaneous network", 
                                "Idiographic temporal networks", "Nomothetic temporal network"), 
                     values = c("#F8766D", "#7CAE00", 
                                "#00BFC4", "#C77CFF")) 


#################################################################################################
# Statistics from figures (Median, Mean, Standard Deviation)
#################################################################################################
# Median 
## Function: Check exact values 
getmedian_lb_ub <- function(simfig_fam1){
  median_boxplot1 <- ggplot_build(simfig_fam1)$data[[1]]
  
  for(i in 1:nrow(median_boxplot1)){
    if (median_boxplot1$fill[i] == "#F8766D"){
      median_boxplot1$network[i] <- "famcontemp"
    } else if (median_boxplot1$fill[i] == "#7CAE00"){
      median_boxplot1$network[i] <- "gencontemp"
    } else if (median_boxplot1$fill[i] == "#00BFC4"){
      median_boxplot1$network[i] <- "famtemp"
    } else {
      median_boxplot1$network[i] <- "gentemp"
    }
  }
  
  # Panel 1: Bias, Panel 2: Correlation, Panel 3: Precision, Panel 4: Sensitivity, Panel 5: Specificity --> Time points: 20  
  # Panel 6: Bias, Panel 7: Correlation, Panel 8: Precision, Panel 9: Sensitivity, Panel 10: Specificity --> Time points: 56  
  # Panel 11: Bias, Panel 12: Correlation, Panel 13: Precision, Panel 14: Sensitivity, Panel 15: Specificity --> Time points: 100  
  # Missing 0: group 1-4, missing 0.1: group 5-8, missing 0.25: group 9-12, missing 0.5: 13-16 
  # Group 1/5/9/13: famcontemp, 2/6/10/14: gencontemp, 3/7/11/15: famtemp, 4/8/12/16: gentemp 
  
  
  for(i in 1:nrow(median_boxplot1)){
    if (median_boxplot1$PANEL[i] == 1 | median_boxplot1$PANEL[i] == 2 | median_boxplot1$PANEL[i] == 3 | median_boxplot1$PANEL[i] == 4 | median_boxplot1$PANEL[i] == 5){
      median_boxplot1$timepoint[i] <- 20
    } else if (median_boxplot1$PANEL[i] == 6 | median_boxplot1$PANEL[i] == 7 | median_boxplot1$PANEL[i] == 8 | median_boxplot1$PANEL[i] == 9 | median_boxplot1$PANEL[i] == 10){
      median_boxplot1$timepoint[i] <- 56
    } else {
      median_boxplot1$timepoint[i] <- 100
    }
  }
  
  for(i in 1:nrow(median_boxplot1)){
    if (median_boxplot1$group[i] == 1 | median_boxplot1$group[i] == 2 | median_boxplot1$group[i] == 3 | median_boxplot1$group[i] == 4){
      median_boxplot1$missing[i] <- 0
    } else if (median_boxplot1$group[i] == 5 | median_boxplot1$group[i] == 6 | median_boxplot1$group[i] == 7 | median_boxplot1$group[i] == 8){
      median_boxplot1$missing[i] <- 0.1
    } else if (median_boxplot1$group[i] == 9 | median_boxplot1$group[i] == 10 | median_boxplot1$group[i] == 11 | median_boxplot1$group[i] == 12){
      median_boxplot1$missing[i] <- 0.25
    } else {
      median_boxplot1$missing[i] <- 0.5
    }
  }
  
  for(i in 1:nrow(median_boxplot1)){
    if (median_boxplot1$PANEL[i] == 1 | median_boxplot1$PANEL[i] == 6 | median_boxplot1$PANEL[i] == 11){
      median_boxplot1$measure[i] <- "Bias"
    } else if (median_boxplot1$PANEL[i] == 2 | median_boxplot1$PANEL[i] == 7 | median_boxplot1$PANEL[i] == 12){
      median_boxplot1$measure[i] <- "Correlation"
    } else if (median_boxplot1$PANEL[i] == 3 | median_boxplot1$PANEL[i] == 8 | median_boxplot1$PANEL[i] == 13){
      median_boxplot1$measure[i] <- "Precision"
    } else if (median_boxplot1$PANEL[i] == 4 | median_boxplot1$PANEL[i] == 9 | median_boxplot1$PANEL[i] == 14){
      median_boxplot1$measure[i] <- "Sensitivity"
    } else if (median_boxplot1$PANEL[i] == 5 | median_boxplot1$PANEL[i] == 10 | median_boxplot1$PANEL[i] == 15){
      median_boxplot1$measure[i] <- "Specificity"
    }
  }
  
  median_boxplot1_short <- median_boxplot1[,c("network", "timepoint", "missing", "measure", 
                                              "ymin", "lower", "middle", "upper", 
                                              "ymax", "outliers", "PANEL", "group")]
  
  return(median_boxplot1_short)
} 

median_boxplot1_short1 <- getmedian_lb_ub(simfig_fam1)  # Fam 30 
median_boxplot2_short2 <- getmedian_lb_ub(simfig_fam2)  # Fam 45 
median_boxplot3_short3 <- getmedian_lb_ub(simfig_fam3)  # Fam 59 

median_boxplot3_short3[median_boxplot3_short3$measure == "Correlation" & 
                         median_boxplot3_short3$network == "gentemp" & 
                         median_boxplot3_short3$timepoint == 100 & 
                         median_boxplot3_short3$missing == 0,]

median_boxplot3_short3[median_boxplot3_short3$measure == "Specificity" & 
                         median_boxplot3_short3$network == "gentemp" & 
                         median_boxplot3_short3$timepoint == 56 & 
                         median_boxplot3_short3$missing == 0.5,]

median_boxplot2_short2[median_boxplot2_short2$measure == "Correlation" & 
                         median_boxplot2_short2$network == "famtemp" & 
                         median_boxplot2_short2$timepoint == 56 & 
                         median_boxplot2_short2$missing == 0.25,]

# Save as .rds  
median_boxplot_comb <- rbind(median_boxplot1_short1, median_boxplot2_short2, median_boxplot3_short3)
saveRDS(median_boxplot_comb, "mediansimulation.rds")


# Mean and Standard Deviation 
## Function 
getmean_sd <- function(mydataframe){
  mean_sd_percondition <- aggregate(mydataframe$value,
                                    list(mydataframe$measure,
                                         mydataframe$network,
                                         mydataframe$nTime, 
                                         mydataframe$nmissing),
                                    function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
  colnames(mean_sd_percondition)[1:4] <- c("Measure", "Network", "Time points", "Missing")
  return(mean_sd_percondition)
}

mean_boxplot1_short1 <- getmean_sd(sim_famnetwork_res_total_13042022_fam1)  # Fam 30 
mean_boxplot2_short2 <- getmean_sd(sim_famnetwork_res_total_13042022_fam2)  # Fam 45 
mean_boxplot3_short3 <- getmean_sd(sim_famnetwork_res_total_13042022_fam3)  # Fam 59 


mean_boxplot3_short3[mean_boxplot3_short3$Measure == "Specificity" & 
                       mean_boxplot3_short3$Network == "temporal_thresholded" & 
                       mean_boxplot3_short3$`Time points` == 56 & 
                       mean_boxplot3_short3$Missing == 0.5,]

mean_boxplot2_short2[mean_boxplot2_short2$Measure == "Correlation" & 
                       mean_boxplot2_short2$Network == "contemporaneous_thresholded" & 
                       mean_boxplot2_short2$`Time points` == 56 & 
                       mean_boxplot2_short2$Missing == 0.25,]


# Save mean and sd information as .rds
mean_boxplot_comb <- rbind(mean_boxplot1_short1, mean_boxplot2_short2, mean_boxplot3_short3)
saveRDS(mean_boxplot_comb, "meansdsimulation.rds")

# Used in paper 
# 1: Variance 
## 1.1 
median_boxplot2_short2[median_boxplot2_short2$measure == "Precision" & 
                         median_boxplot2_short2$network == "gentemp" & 
                         median_boxplot2_short2$timepoint == 56 & 
                         median_boxplot2_short2$missing == 0,]

mean_boxplot2_short2[mean_boxplot2_short2$Measure == "Precision" & 
                       mean_boxplot2_short2$Network == "temporal_thresholded" & 
                       mean_boxplot2_short2$`Time points` == 56 & 
                       mean_boxplot2_short2$Missing == 0,]

## 1.2 
median_boxplot2_short2[median_boxplot2_short2$measure == "Precision" & 
                         median_boxplot2_short2$network == "gentemp" & 
                         median_boxplot2_short2$timepoint == 56 & 
                         median_boxplot2_short2$missing == 0.5,]

mean_boxplot2_short2[mean_boxplot2_short2$Measure == "Precision" & 
                       mean_boxplot2_short2$Network == "temporal_thresholded" & 
                       mean_boxplot2_short2$`Time points` == 56 & 
                       mean_boxplot2_short2$Missing == 0.5,]
# 2: Difference temporal and contemporaneous 
## 2.1 
median_boxplot2_short2[median_boxplot2_short2$measure == "Sensitivity" & 
                         median_boxplot2_short2$network == "gentemp" & 
                         median_boxplot2_short2$timepoint == 100 & 
                         median_boxplot2_short2$missing == 0.1,]

mean_boxplot2_short2[mean_boxplot2_short2$Measure == "Sensitivity" & 
                       mean_boxplot2_short2$Network == "temporal_thresholded" & 
                       mean_boxplot2_short2$`Time points` == 100 & 
                       mean_boxplot2_short2$Missing == 0.1,]
## 2.2 
median_boxplot2_short2[median_boxplot2_short2$measure == "Sensitivity" & 
                         median_boxplot2_short2$network == "gencontemp" & 
                         median_boxplot2_short2$timepoint == 100 & 
                         median_boxplot2_short2$missing == 0.1,]

mean_boxplot2_short2[mean_boxplot2_short2$Measure == "Sensitivity" & 
                       mean_boxplot2_short2$Network == "contemporaneous_thresholded" & 
                       mean_boxplot2_short2$`Time points` == 100 & 
                       mean_boxplot2_short2$Missing == 0.1,]

# 3: Bias  
## 3.1 
median_boxplot2_short2[median_boxplot2_short2$measure == "Bias" & 
                         median_boxplot2_short2$network == "famcontemp" & 
                         median_boxplot2_short2$timepoint == 56 & 
                         median_boxplot2_short2$missing == 0.25,]

mean_boxplot2_short2[mean_boxplot2_short2$Measure == "Bias" & 
                       mean_boxplot2_short2$Network == "contemporaneous_subject" & 
                       mean_boxplot2_short2$`Time points` == 56 & 
                       mean_boxplot2_short2$Missing == 0.25,]

## 3.2 
median_boxplot2_short2[median_boxplot2_short2$measure == "Bias" & 
                         median_boxplot2_short2$network == "gencontemp" & 
                         median_boxplot2_short2$timepoint == 56 & 
                         median_boxplot2_short2$missing == 0.25,]

mean_boxplot2_short2[mean_boxplot2_short2$Measure == "Bias" & 
                       mean_boxplot2_short2$Network == "contemporaneous_thresholded" & 
                       mean_boxplot2_short2$`Time points` == 56 & 
                       mean_boxplot2_short2$Missing == 0.25,]

# 4: Correlation 
## 4.1 
median_boxplot3_short3[median_boxplot3_short3$measure == "Correlation" & 
                         median_boxplot3_short3$network == "gentemp" & 
                         median_boxplot3_short3$timepoint == 100 & 
                         median_boxplot3_short3$missing == 0,]
## 4.2 
median_boxplot3_short3[median_boxplot3_short3$measure == "Correlation" & 
                         median_boxplot3_short3$network == "gencontemp" & 
                         median_boxplot3_short3$timepoint == 100 & 
                         median_boxplot3_short3$missing == 0,]
## 4.3 
median_boxplot2_short2[median_boxplot2_short2$measure == "Correlation" & 
                         median_boxplot2_short2$network == "gentemp" & 
                         median_boxplot2_short2$timepoint == 56 & 
                         median_boxplot2_short2$missing == 0.25,]

mean_boxplot2_short2[mean_boxplot2_short2$Measure == "Correlation" & 
                       mean_boxplot2_short2$Network == "temporal_thresholded" & 
                       mean_boxplot2_short2$`Time points` == 56 & 
                       mean_boxplot2_short2$Missing == 0.25,]
## 4.4 
median_boxplot2_short2[median_boxplot2_short2$measure == "Correlation" & 
                         median_boxplot2_short2$network == "famtemp" & 
                         median_boxplot2_short2$timepoint == 56 & 
                         median_boxplot2_short2$missing == 0.25,]

mean_boxplot2_short2[mean_boxplot2_short2$Measure == "Correlation" & 
                       mean_boxplot2_short2$Network == "temporal_subject" & 
                       mean_boxplot2_short2$`Time points` == 56 & 
                       mean_boxplot2_short2$Missing == 0.25,]
# 5: Precision 
## 5.1. 
mean_boxplot3_short3[mean_boxplot3_short3$Measure == "Precision" & 
                       mean_boxplot3_short3$Network == "temporal_thresholded" & 
                       mean_boxplot3_short3$`Time points` == 56 & 
                       mean_boxplot3_short3$Missing == 0.5,]
## 5.2 
mean_boxplot3_short3[mean_boxplot3_short3$Measure == "Precision" & 
                       mean_boxplot3_short3$Network == "temporal_thresholded" & 
                       mean_boxplot3_short3$`Time points` == 56 & 
                       mean_boxplot3_short3$Missing == 0,]
# 6: Sensitivity 
## 6.1 
median_boxplot3_short3[median_boxplot3_short3$measure == "Sensitivity" & 
                         median_boxplot3_short3$network == "gentemp" & 
                         median_boxplot3_short3$timepoint == 56 & 
                         median_boxplot3_short3$missing == 0,]

mean_boxplot3_short3[mean_boxplot3_short3$Measure == "Sensitivity" & 
                       mean_boxplot3_short3$Network == "temporal_thresholded" & 
                       mean_boxplot3_short3$`Time points` == 56 & 
                       mean_boxplot3_short3$Missing == 0,]
## 6.2 
median_boxplot3_short3[median_boxplot3_short3$measure == "Sensitivity" & 
                         median_boxplot3_short3$network == "gencontemp" & 
                         median_boxplot3_short3$timepoint == 56 & 
                         median_boxplot3_short3$missing == 0,]

mean_boxplot3_short3[mean_boxplot3_short3$Measure == "Sensitivity" & 
                       mean_boxplot3_short3$Network == "contemporaneous_thresholded" & 
                       mean_boxplot3_short3$`Time points` == 56 & 
                       mean_boxplot3_short3$Missing == 0,]
# 7 : Specificity 
## 7.1 
median_boxplot3_short3[median_boxplot3_short3$measure == "Specificity" & 
                         median_boxplot3_short3$network == "gentemp" & 
                         median_boxplot3_short3$timepoint == 56 & 
                         median_boxplot3_short3$missing == 0,]

mean_boxplot3_short3[mean_boxplot3_short3$Measure == "Specificity" & 
                       mean_boxplot3_short3$Network == "temporal_thresholded" & 
                       mean_boxplot3_short3$`Time points` == 56 & 
                       mean_boxplot3_short3$Missing == 0,]
## 7.2 
median_boxplot3_short3[median_boxplot3_short3$measure == "Specificity" & 
                         median_boxplot3_short3$network == "gentemp" & 
                         median_boxplot3_short3$timepoint == 56 & 
                         median_boxplot3_short3$missing == 0.5,]

mean_boxplot3_short3[mean_boxplot3_short3$Measure == "Specificity" & 
                       mean_boxplot3_short3$Network == "temporal_thresholded" & 
                       mean_boxplot3_short3$`Time points` == 56 & 
                       mean_boxplot3_short3$Missing == 0.5,]
