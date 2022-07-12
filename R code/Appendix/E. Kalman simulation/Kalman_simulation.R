################################################################################
# Kalman simulation 
################################################################################
# Load packages 
library(parSim) 
library(imputeTS)

# Simulation 
## %'s missingness 
## without rounding 
Kalman_sim <- parSim(
  missing = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9),  # 10%, 25%, 50% (percentage missing data)
  reps = 1000,   # 1000 repetitions per condition (missingness)
  expression = {
    
    true_data <- readRDS("myPath/moodp_oneperson_sub.rds")  # Load true data set - unfortunately this data is not openly available
    
    est_data <- true_data
    # missingness 
    total_obs <- nrow(true_data)
    rows_na <- sample(nrow(true_data), missing * total_obs)  # select random rows 
    est_data[rows_na,c(1:ncol(est_data))] <- NA   # set the random rows to NA - missing data 
    
    # apply Kalman filter imputation 
    est_data <- bind_cols(lapply(est_data, na_kalman, type = "level"))
    
    # Calculate correlation between true data and estimated data 
    res_cor <- bind_rows(diag(cor(true_data, est_data)))
    
    return(res_cor)
  }
)

## Plot results Kalman simulation 
Kalman_sim_h<- data.frame(missing = Kalman_sim$missing, rep = Kalman_sim$rep, 
                          id = Kalman_sim$id, value = Kalman_sim$HAPPY,
                          name = rep("HAPPY", nrow(Kalman_sim)))
Kalman_sim_s<- data.frame(missing = Kalman_sim$missing, rep = Kalman_sim$rep, 
                          id = Kalman_sim$id, value = Kalman_sim$SAD,
                          name = rep("SAD", nrow(Kalman_sim)))
Kalman_sim_r<- data.frame(missing = Kalman_sim$missing, rep = Kalman_sim$rep, 
                          id = Kalman_sim$id, value = Kalman_sim$RELAX,
                          name = rep("RELAX", nrow(Kalman_sim)))
Kalman_sim_i<- data.frame(missing = Kalman_sim$missing, rep = Kalman_sim$rep, 
                          id = Kalman_sim$id, value = Kalman_sim$IRRI,
                          name = rep("IRRI", nrow(Kalman_sim)))

Kalman_sim_long <- rbind(Kalman_sim_h, Kalman_sim_s, Kalman_sim_r, Kalman_sim_i)

ggplot(Kalman_sim_long, aes(x = as.factor(missing), y = value, fill = name)) + 
  geom_boxplot() + 
  xlab("Missingness (%)") + 
  ylab("Correlation") + 
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey", size = .3) + 
  geom_hline(yintercept = 0, color = "grey", size = .3) + 
  guides(fill=guide_legend(title="Affect")) + 
  theme_classic()

# This code can also be found in a separate R file in folder "Figures"
ggplot(Kalman_sim_long, aes(x = as.factor(missing), y = value)) + 
  geom_boxplot() + 
  xlab("Missingness (%)") + 
  ylab("Correlation") + 
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey", size = .3) + 
  geom_hline(yintercept = 0, color = "grey", size = .3) + 
  guides(fill=guide_legend(title="Affect")) + 
  theme_classic() + 
  scale_y_continuous(n.breaks = 10)


## simulation with rounding 
Kalman_sim_round <- parSim(
  missing = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9),  # 10%, 25%, 50% (percentage missing data)
  reps = 1000,    # number of repetitions per condition (percentage missing data)
  expression = {
    
    true_data <- readRDS("myPath/moodp_oneperson_sub.rds")  # Load true data set - unfortunately this data is not openly available
    
    
    est_data <- true_data
    # missingness 
    total_obs <- nrow(true_data)
    rows_na <- sample(nrow(true_data), missing * total_obs)  # select random rows 
    est_data[rows_na,c(1:ncol(est_data))] <- NA    # set the random rows to NA - missing data 
    
    # apply Kalman filter imputation 
    est_data <- bind_cols(lapply(est_data, na_kalman, type = "level"))
    est_data <- round(est_data, 0)   # round to get categorical data 
    print(est_data)
    # Calculate correlation between true data and estimated data 
    res_cor <- bind_rows(diag(cor(true_data, est_data)))
    
    return(res_cor)
  }
)

## Plot results Kalman simulation 

plot_Kalman_sim <- function(Kalman_sim){
  Kalman_sim_h<- data.frame(missing = Kalman_sim$missing, rep = Kalman_sim$rep, 
                            id = Kalman_sim$id, value = Kalman_sim$HAPPY,
                            name = rep("HAPPY", nrow(Kalman_sim)))
  Kalman_sim_s<- data.frame(missing = Kalman_sim$missing, rep = Kalman_sim$rep, 
                            id = Kalman_sim$id, value = Kalman_sim$SAD,
                            name = rep("SAD", nrow(Kalman_sim)))
  Kalman_sim_r<- data.frame(missing = Kalman_sim$missing, rep = Kalman_sim$rep, 
                            id = Kalman_sim$id, value = Kalman_sim$RELAX,
                            name = rep("RELAX", nrow(Kalman_sim)))
  Kalman_sim_i<- data.frame(missing = Kalman_sim$missing, rep = Kalman_sim$rep, 
                            id = Kalman_sim$id, value = Kalman_sim$IRRI,
                            name = rep("IRRI", nrow(Kalman_sim)))
  
  Kalman_sim_long <- rbind(Kalman_sim_h, Kalman_sim_s, Kalman_sim_r, Kalman_sim_i)
  
  ggplot(Kalman_sim_long, aes(x = as.factor(missing), y = value, fill = name)) + 
    geom_boxplot() + 
    xlab("Missingness (%)") + 
    ylab("Correlation") + 
    geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey", size = .3) + 
    geom_hline(yintercept = 0, color = "grey", size = .3) + 
    guides(fill=guide_legend(title="Affect")) + 
    theme_classic()
} 
plot_Kalman_sim(Kalman_sim_round)
