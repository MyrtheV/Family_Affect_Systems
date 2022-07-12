# Kalman filter functions 
library("imputeTS") 
library(ggplot2)

ema_imp <- function(data, individuals, variables){
  #mooda_ind_var <- matrix(NA, nrow = 56, ncol = length(variables))
  mooda_ind_imp <- list()
  
  for(i in 1:length(unique(individuals))){
    mooda_ind <- data[individuals == unique(individuals)[i], variables]
    mooda_ind_var <- matrix(NA, nrow = nrow(mooda_ind), ncol = length(variables))
    
    for(j in 1:length(variables)){
      if (length(is.na(mooda_ind[,j]))==0){  # if no na for variable than do nothing 
        mooda_ind_var[,j] <- mooda_ind[,j]  
      }else if (length(unique(mooda_ind[,j]))<=2){  # if only two unique values including na, then set na to other available value 
        mooda_ind_var[,j] <- min(unique(mooda_ind[,j]), na.rm = TRUE)
      }else{
        mooda_ind_var[,j] <- na_kalman(mooda_ind[,j], type = "level")
      }
    }
    mooda_ind_imp[[i]] <- mooda_ind_var
    
  }
  mooda_imp <- do.call(rbind, mooda_ind_imp)  # to create a nice dataframe again instead of a list you can do this 
  mooda_imp <- cbind(data, mooda_imp)
  
  for(k in 1:length(variables)){
    colnames(mooda_imp)[which(names(mooda_imp) == as.character(k))] <- paste(variables[k], "_i", sep = "")
  }
  
  imputation <- matrix(NA, nrow = nrow(mooda_imp), ncol = length(variables))
  for(l in 1:length(variables)){
    for(m in 1:nrow(mooda_imp)){
      if(is.na(mooda_imp[m, variables[l]])){
        imputation[m,l] <- "imputed values"
      }else{
        imputation[m,l] <- "known values"
      }
    }
  }
  mooda_imp <- cbind(mooda_imp, imputation)
  
  for(n in 1:length(variables)){
    colnames(mooda_imp)[which(names(mooda_imp) == as.character(n))] <- paste(variables[n], "_group", sep = "")
  }
  
  return(mooda_imp)
}

# Functions for plots 
ema_imp_plot <- function(data, individuals, name){
  pdf(name, onefile = TRUE)
  
  for(i in 1:length(unique(individuals))){
    ### change data, only use the legend 
    mooda_imp1 <- data[individuals == unique(individuals)[i],]
    happyplot1 <- data.frame(obs = mooda_imp1$obs, affect = mooda_imp1$HAPPY_i, values = mooda_imp1$HAPPY_group, 
                             name = rep("Happy", nrow(mooda_imp1)))
    sadplot1 <- data.frame(obs = mooda_imp1$obs, affect = mooda_imp1$SAD_i, values = mooda_imp1$SAD_group, 
                           name = rep("Sad", nrow(mooda_imp1)))
    relaxplot1 <- data.frame(obs = mooda_imp1$obs, affect = mooda_imp1$RELAX_i, values = mooda_imp1$RELAX_group, 
                             name = rep("Relaxed", nrow(mooda_imp1)))
    irriplot1 <- data.frame(obs = mooda_imp1$obs, affect = mooda_imp1$IRRI_i, values = mooda_imp1$IRRI_group, 
                            name = rep("Irritated", nrow(mooda_imp1)))
    #### columns under each other 
    mooda_imp1_long <- rbind(happyplot1, sadplot1, relaxplot1, irriplot1)
    mooda_imp1_long$type <- paste(mooda_imp1_long$name, mooda_imp1_long$values, sep=' ')
    
    ### Plot to obtain nice legend 
    after_imp22 <- ggplot(mooda_imp1_long, 
                          aes(x = obs, y = affect, colour = type, shape = type)) + 
      geom_line() + 
      geom_point(aes(alpha = 0.3)) + 
      xlab("Observation") + 
      ylab("Score") + 
      theme_classic() + 
      scale_x_continuous(breaks = c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56)) + 
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) + 
      geom_vline(xintercept = c(28), linetype = "dashed", color = "grey", size = .3) + 
      scale_alpha(guide = "none") + 
      scale_colour_manual(name = "Affect",
                          labels = c("Happy imputed values", 
                                     "Happy known values", 
                                     "Sad imputed values", 
                                     "Sad known values", 
                                     "Relaxed imputed values", 
                                     "Relaxed known values",
                                     "Irritated imputed values", 
                                     "Irritated known values"),
                          values = c("#F8766D", "#F8766D", "#00BFC4", "#00BFC4",
                                     "#C77CFF",  "#C77CFF", "#7CAE00", "#7CAE00")) +   
      scale_shape_manual(name = "Affect",
                         labels = c("Happy imputed values", 
                                    "Happy known values", 
                                    "Sad imputed values", 
                                    "Sad known values", 
                                    "Relaxed imputed values", 
                                    "Relaxed known values",
                                    "Irritated imputed values", 
                                    "Irritated known values"),
                         values = rep(c(8, 19), 4))
    
    
    legend_after_imp22 <- get_legend(after_imp22)
    
    #### Actual plot 
    ### this works but legend not ideal 
    after_imp2 <- ggplot(data[individuals == unique(individuals)[i],]) + 
      geom_line(aes(y = HAPPY_i, x = obs, colour = "Happy")) + 
      geom_line(aes(y = RELAX_i, x = obs, colour = "Relaxed")) + 
      geom_line(aes(y = SAD_i, x = obs, colour = "Sad")) + 
      geom_line(aes(y = IRRI_i, x = obs, colour = "Irritated")) + 
      geom_point(aes(y = HAPPY_i, x = obs, 
                     shape= HAPPY_group, alpha = 0.3, colour = "Happy")) + 
      geom_point(aes(y = RELAX_i, x = obs, 
                     shape= RELAX_group, alpha = 0.3, colour = "Relaxed")) + 
      geom_point(aes(y = SAD_i, x = obs, 
                     shape= SAD_group, alpha = 0.3, colour = "Sad")) + 
      geom_point(aes(y = IRRI_i, x = obs,
                     shape= IRRI_group, alpha = 0.3, colour = "Irritated")) + 
      xlab("Observation") + 
      ylab("Score") + 
      theme_classic() + 
      # guides(colour=guide_legend(title="Affect"), shape = guide_legend(title="")) + 
      # labs(subtitle = "") + 
      scale_x_continuous(breaks = c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56)) + 
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) + 
      geom_vline(xintercept = c(28), linetype = "dashed", color = "grey", size = .3) + 
      #scale_shape_manual(values=c(8,19)) +  # 20 for circle white filling, 18  
      scale_alpha(guide = "none") + 
      scale_colour_manual(name = "Affect",
                          labels = c("Happy known values", 
                                     "Relaxed known values", 
                                     "Sad known values", 
                                     "Irritated known values", 
                                     "Happy imputed values",
                                     "Relaxed imputed values", 
                                     "Sad imputed values", 
                                     "Irritated imputed values"),
                          values = c("#F8766D", "#00BFC4", "#C77CFF", "#7CAE00", 
                                     "#F8766D", "#00BFC4", "#C77CFF", "#7CAE00")) +   
      scale_shape_manual(name = "Affect",
                         labels = c("Happy known values", 
                                    "Happy imputed values", 
                                    "Sad known values", 
                                    "Sad imputed values", 
                                    "Relaxed known values", 
                                    "Relaxed imputed values", 
                                    "Irritated known values", 
                                    "Irritated imputed values"),
                         values = rep(c(8, 19), 4)) + 
      theme(legend.position = "none")
    
    grid.arrange(after_imp2, legend_after_imp22, ncol = 2, widths=c(3.5,1.5))
    # after_imp_plot[[i]] <- arrangeGrob(after_imp2, legend_after_imp22, ncol = 2, widths=c(5/6,1/6))
  }
  dev.off()
  
}

ema_impx_plot <- function(data, individuals, name){
  pdf(name, onefile = TRUE)
  
  for(i in 1:length(unique(individuals))){
    ### change data, only use the legend 
    mooda_imp1 <- data[individuals == unique(individuals)[i],]
    happyplot1 <- data.frame(obs = mooda_imp1$obs, affect = mooda_imp1$HAPPY_i, values = mooda_imp1$HAPPY_group, 
                             name = rep("Happy", nrow(mooda_imp1)))
    sadplot1 <- data.frame(obs = mooda_imp1$obs, affect = mooda_imp1$SAD_i, values = mooda_imp1$SAD_group, 
                           name = rep("Sad", nrow(mooda_imp1)))
    relaxplot1 <- data.frame(obs = mooda_imp1$obs, affect = mooda_imp1$RELAX_i, values = mooda_imp1$RELAX_group, 
                             name = rep("Relaxed", nrow(mooda_imp1)))
    irriplot1 <- data.frame(obs = mooda_imp1$obs, affect = mooda_imp1$IRRI_i, values = mooda_imp1$IRRI_group, 
                            name = rep("Irritated", nrow(mooda_imp1)))
    #### columns under each other 
    mooda_imp1_long <- rbind(happyplot1, sadplot1, relaxplot1, irriplot1)
    mooda_imp1_long$type <- paste(mooda_imp1_long$name, mooda_imp1_long$values, sep=' ')
    
    ### Plot to obtain nice legend 
    after_imp22 <- ggplot(mooda_imp1_long, 
                          aes(x = obs, y = affect, colour = type, shape = type)) + 
      geom_line() + 
      geom_point(aes(alpha = 0.3)) + 
      xlab("Observation") + 
      ylab("Score") + 
      theme_classic() + 
      scale_x_continuous(breaks = c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56)) + 
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) + 
      geom_vline(xintercept = c(28), linetype = "dashed", color = "grey", size = .3) + 
      scale_alpha(guide = "none") + 
      scale_colour_manual(name = "Affect",
                          labels = c("Happy imputed values", 
                                     "Happy known values", 
                                     "Sad imputed values", 
                                     "Sad known values", 
                                     "Relaxed imputed values", 
                                     "Relaxed known values",
                                     "Irritated imputed values", 
                                     "Irritated known values"),
                          values = c("#F8766D", "#F8766D", "#00BFC4", "#00BFC4",
                                     "#C77CFF",  "#C77CFF", "#7CAE00", "#7CAE00")) +   
      scale_shape_manual(name = "Affect",
                         labels = c("Happy imputed values", 
                                    "Happy known values", 
                                    "Sad imputed values", 
                                    "Sad known values", 
                                    "Relaxed imputed values", 
                                    "Relaxed known values",
                                    "Irritated imputed values", 
                                    "Irritated known values"),
                         values = rep(c(8, 19), 4))
    
    
    legend_after_imp22 <- get_legend(after_imp22)
    
    #### Actual plot 
    ### this works but legend not ideal 
    after_imp2 <- ggplot(data[individuals == unique(individuals)[i],]) + 
      geom_line(aes(y = HAPPY.x_i, x = obs, colour = "Happy")) + 
      geom_line(aes(y = RELAX.x_i, x = obs, colour = "Relaxed")) + 
      geom_line(aes(y = SAD.x_i, x = obs, colour = "Sad")) + 
      geom_line(aes(y = IRRI.x_i, x = obs, colour = "Irritated")) + 
      geom_point(aes(y = HAPPY.x_i, x = obs, 
                     shape= HAPPY.x_group, alpha = 0.3, colour = "Happy")) + 
      geom_point(aes(y = RELAX.x_i, x = obs, 
                     shape= RELAX.x_group, alpha = 0.3, colour = "Relaxed")) + 
      geom_point(aes(y = SAD.x_i, x = obs, 
                     shape= SAD.x_group, alpha = 0.3, colour = "Sad")) + 
      geom_point(aes(y = IRRI.x_i, x = obs,
                     shape= IRRI.x_group, alpha = 0.3, colour = "Irritated")) + 
      xlab("Observation") + 
      ylab("Score") + 
      theme_classic() + 
      # guides(colour=guide_legend(title="Affect"), shape = guide_legend(title="")) + 
      # labs(subtitle = "") + 
      scale_x_continuous(breaks = c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56)) + 
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) + 
      geom_vline(xintercept = c(28), linetype = "dashed", color = "grey", size = .3) + 
      #scale_shape_manual(values=c(8,19)) +  # 20 for circle white filling, 18  
      scale_alpha(guide = "none") + 
      scale_colour_manual(name = "Affect",
                          labels = c("Happy known values", 
                                     "Relaxed known values", 
                                     "Sad known values", 
                                     "Irritated known values", 
                                     "Happy imputed values",
                                     "Relaxed imputed values", 
                                     "Sad imputed values", 
                                     "Irritated imputed values"),
                          values = c("#F8766D", "#00BFC4", "#C77CFF", "#7CAE00", 
                                     "#F8766D", "#00BFC4", "#C77CFF", "#7CAE00")) +   
      scale_shape_manual(name = "Affect",
                         labels = c("Happy known values", 
                                    "Happy imputed values", 
                                    "Sad known values", 
                                    "Sad imputed values", 
                                    "Relaxed known values", 
                                    "Relaxed imputed values", 
                                    "Irritated known values", 
                                    "Irritated imputed values"),
                         values = rep(c(8, 19), 4)) + 
      theme(legend.position = "none")
    
    grid.arrange(after_imp2, legend_after_imp22, ncol = 2, widths=c(3.5,1.5))
    # after_imp_plot[[i]] <- arrangeGrob(after_imp2, legend_after_imp22, ncol = 2, widths=c(5/6,1/6))
  }
  dev.off()
  
}

ema_impyy_plot <- function(data, individuals, name){
  pdf(name, onefile = TRUE)
  
  for(i in 1:length(unique(individuals))){
    ### change data, only use the legend 
    mooda_imp1 <- data[individuals == unique(individuals)[i],]
    happyplot1 <- data.frame(obs = mooda_imp1$obs, affect = mooda_imp1$HAPPY_i, values = mooda_imp1$HAPPY_group, 
                             name = rep("Happy", nrow(mooda_imp1)))
    sadplot1 <- data.frame(obs = mooda_imp1$obs, affect = mooda_imp1$SAD_i, values = mooda_imp1$SAD_group, 
                           name = rep("Sad", nrow(mooda_imp1)))
    relaxplot1 <- data.frame(obs = mooda_imp1$obs, affect = mooda_imp1$RELAX_i, values = mooda_imp1$RELAX_group, 
                             name = rep("Relaxed", nrow(mooda_imp1)))
    irriplot1 <- data.frame(obs = mooda_imp1$obs, affect = mooda_imp1$IRRI_i, values = mooda_imp1$IRRI_group, 
                            name = rep("Irritated", nrow(mooda_imp1)))
    #### columns under each other 
    mooda_imp1_long <- rbind(happyplot1, sadplot1, relaxplot1, irriplot1)
    mooda_imp1_long$type <- paste(mooda_imp1_long$name, mooda_imp1_long$values, sep=' ')
    
    ### Plot to obtain nice legend 
    after_imp22 <- ggplot(mooda_imp1_long, 
                          aes(x = obs, y = affect, colour = type, shape = type)) + 
      geom_line() + 
      geom_point(aes(alpha = 0.3)) + 
      xlab("Observation") + 
      ylab("Score") + 
      theme_classic() + 
      scale_x_continuous(breaks = c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56)) + 
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) + 
      geom_vline(xintercept = c(28), linetype = "dashed", color = "grey", size = .3) + 
      scale_alpha(guide = "none") + 
      scale_colour_manual(name = "Affect",
                          labels = c("Happy imputed values", 
                                     "Happy known values", 
                                     "Sad imputed values", 
                                     "Sad known values", 
                                     "Relaxed imputed values", 
                                     "Relaxed known values",
                                     "Irritated imputed values", 
                                     "Irritated known values"),
                          values = c("#F8766D", "#F8766D", "#00BFC4", "#00BFC4",
                                     "#C77CFF",  "#C77CFF", "#7CAE00", "#7CAE00")) +   
      scale_shape_manual(name = "Affect",
                         labels = c("Happy imputed values", 
                                    "Happy known values", 
                                    "Sad imputed values", 
                                    "Sad known values", 
                                    "Relaxed imputed values", 
                                    "Relaxed known values",
                                    "Irritated imputed values", 
                                    "Irritated known values"),
                         values = rep(c(8, 19), 4))
    
    
    legend_after_imp22 <- get_legend(after_imp22)
    
    #### Actual plot 
    ### this works but legend not ideal 
    after_imp2 <- ggplot(data[individuals == unique(individuals)[i],]) + 
      geom_line(aes(y = HAPPY.y_i, x = obs, colour = "Happy")) + 
      geom_line(aes(y = RELAX.y_i, x = obs, colour = "Relaxed")) + 
      geom_line(aes(y = SAD.y_i, x = obs, colour = "Sad")) + 
      geom_line(aes(y = IRRI.y_i, x = obs, colour = "Irritated")) + 
      geom_point(aes(y = HAPPY.y_i, x = obs, 
                     shape= HAPPY.y_group, alpha = 0.3, colour = "Happy")) + 
      geom_point(aes(y = RELAX.y_i, x = obs, 
                     shape= RELAX.y_group, alpha = 0.3, colour = "Relaxed")) + 
      geom_point(aes(y = SAD.y_i, x = obs, 
                     shape= SAD.y_group, alpha = 0.3, colour = "Sad")) + 
      geom_point(aes(y = IRRI.y_i, x = obs,
                     shape= IRRI.y_group, alpha = 0.3, colour = "Irritated")) + 
      xlab("Observation") + 
      ylab("Score") + 
      theme_classic() + 
      # guides(colour=guide_legend(title="Affect"), shape = guide_legend(title="")) + 
      # labs(subtitle = "") + 
      scale_x_continuous(breaks = c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56)) + 
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) + 
      geom_vline(xintercept = c(28), linetype = "dashed", color = "grey", size = .3) + 
      #scale_shape_manual(values=c(8,19)) +  # 20 for circle white filling, 18  
      scale_alpha(guide = "none") + 
      scale_colour_manual(name = "Affect",
                          labels = c("Happy known values", 
                                     "Relaxed known values", 
                                     "Sad known values", 
                                     "Irritated known values", 
                                     "Happy imputed values",
                                     "Relaxed imputed values", 
                                     "Sad imputed values", 
                                     "Irritated imputed values"),
                          values = c("#F8766D", "#00BFC4", "#C77CFF", "#7CAE00", 
                                     "#F8766D", "#00BFC4", "#C77CFF", "#7CAE00")) +   
      scale_shape_manual(name = "Affect",
                         labels = c("Happy known values", 
                                    "Happy imputed values", 
                                    "Sad known values", 
                                    "Sad imputed values", 
                                    "Relaxed known values", 
                                    "Relaxed imputed values", 
                                    "Irritated known values", 
                                    "Irritated imputed values"),
                         values = rep(c(8, 19), 4)) + 
      theme(legend.position = "none")
    
    grid.arrange(after_imp2, legend_after_imp22, ncol = 2, widths=c(3.5,1.5))
    # after_imp_plot[[i]] <- arrangeGrob(after_imp2, legend_after_imp22, ncol = 2, widths=c(5/6,1/6))
  }
  dev.off()
  
}