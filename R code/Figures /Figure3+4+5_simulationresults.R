# Figure 3 + 4 + 5: simulation results 

# Load packages 
library(mlVAR)
library(qgraph)
library(ggplot2)

# Load results simulation 
sim_famnetwork_res_total <- read.csv("myPath/R objects/Simulation/Results/sim_famnetwork_res_total_13042022.csv")

# Function to select the results of the idiographic and nomothetic networks  
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

# Results for idiographic and nomothetic networks only 
sim_famnetwork_res_total_13042022_short <- shortdatasetpermeasure_func(sim_famnetwork_res_total_13042022)

# Function to save figures 
## Saves the figures of different number of families 
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

# Apply function and save the figures results simulation condition per number of families 
simulationfigurefunc(sim_famnetwork_res_total_13042022_short$all)  # saves the three figures per number of families as .png files in the directory you are working in 

# Or don't use the function and fit them separately 
## Number of families - labels facet_grid 
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

