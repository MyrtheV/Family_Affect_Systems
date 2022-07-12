# Figures: response trajectories families 

# Load packages 
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)

# Affect of family members in same plot per affect state
# resulting in 4 figures per family 
ema_imp_plot3 <- function(data, individuals, familyi){
  
  #### Actual plot 
  ### this works but legend not ideal 
  data <- data[individuals == unique(individuals)[familyi],]
  after_imp2 <- ggplot(data) + 
    geom_line(aes(y = HAPPY_i, x = obs, colour = "Happy father")) + 
    geom_line(aes(y = HAPPY.y_i, x = obs, colour = "Happy mother")) + 
    geom_line(aes(y = HAPPY.x_i, x = obs, colour = "Happy adolescent")) + 
    geom_point(data = data[data$HAPPY_group == "imputed values",], aes(y = HAPPY_i, x = obs, 
                                                                       shape = "Happy father - imputed", alpha = 0.3, colour = "Happy father - imputed")) + 
    geom_point(data = data[data$HAPPY_group == "known values",], aes(y = HAPPY_i, x = obs, 
                                                                     shape = "Happy father", alpha = 0.3, colour = "Happy father")) + 
    geom_point(data = data[data$HAPPY.y_group == "imputed values",], aes(y = HAPPY.y_i, x = obs, 
                                                                         shape = "Happy mother - imputed", alpha = 0.3, colour = "Happy mother - imputed")) + 
    geom_point(data = data[data$HAPPY.y_group == "known values",], aes(y = HAPPY.y_i, x = obs, 
                                                                       shape = "Happy mother", alpha = 0.3, colour = "Happy mother")) + 
    geom_point(data = data[data$HAPPY.x_group == "imputed values",], aes(y = HAPPY.x_i, x = obs, 
                                                                         shape = "Happy adolescent - imputed", alpha = 0.3, colour = "Happy adolescent - imputed")) + 
    geom_point(data = data[data$HAPPY.x_group == "known values",], aes(y = HAPPY.x_i, x = obs, 
                                                                       shape = "Happy adolescent", alpha = 0.3, colour = "Happy adolescent")) + 
    xlab("") + 
    ylab("Score") + 
    theme_classic() + 
    # guides(colour=guide_legend(title="Affect"), shape = guide_legend(title="")) + 
    # labs(subtitle = "") + 
    scale_x_continuous(breaks = c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56)) + 
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) + 
    geom_vline(xintercept = c(28), linetype = "dashed", color = "grey", size = .3)  + 
    #scale_shape_manual(values=c(8,19)) +  # 20 for circle white filling, 18  
    scale_alpha(guide = "none") + 
    scale_colour_manual(name = "Affect",                               # works alphabetically 
                        labels = c("Happy adolescent", 
                                   "Happy adolescent - imputed",
                                   "Happy father",
                                   "Happy father - imputed", 
                                   "Happy mother",
                                   "Happy mother - imputed"),
                        values = c("#E69F00", "#E69F00", "#56B4E9", "#56B4E9", "#009E73", "#009E73")) + 
    scale_shape_manual(name = "Affect",
                       labels = c("Happy adolescent",
                                  "Happy adolescent - imputed",
                                  "Happy father",
                                  "Happy father - imputed", 
                                  "Happy mother",
                                  "Happy mother - imputed"),
                       values = c(19, 8, 19, 8, 19, 8))
  #theme(legend.position = "none")
  
  return(after_imp2)
  # grid.arrange(after_imp2, legend_after_imp22, ncol = 2, widths=c(3.5,1.5))
  # after_imp_plot[[i]] <- arrangeGrob(after_imp2, legend_after_imp22, ncol = 2, widths=c(5/6,1/6))
}

ema_imp_plotsad3 <- function(data, individuals, familyi){
  
  #### Actual plot 
  ### this works but legend not ideal 
  data <- data[individuals == unique(individuals)[familyi],]
  after_imp2 <- ggplot(data) + 
    geom_line(aes(y = SAD_i, x = obs, colour = "Sad father")) + 
    geom_line(aes(y = SAD.y_i, x = obs, colour = "Sad mother")) + 
    geom_line(aes(y = SAD.x_i, x = obs, colour = "Sad adolescent")) + 
    geom_point(data = data[data$SAD_group == "imputed values",], aes(y = SAD_i, x = obs, 
                                                                     shape = "Sad father - imputed", alpha = 0.3, colour = "Sad father - imputed")) + 
    geom_point(data = data[data$SAD_group == "known values",], aes(y = SAD_i, x = obs, 
                                                                   shape = "Sad father", alpha = 0.3, colour = "Sad father")) + 
    geom_point(data = data[data$SAD.y_group == "imputed values",], aes(y = SAD.y_i, x = obs, 
                                                                       shape = "Sad mother - imputed", alpha = 0.3, colour = "Sad mother - imputed")) + 
    geom_point(data = data[data$SAD.y_group == "known values",], aes(y = SAD.y_i, x = obs, 
                                                                     shape = "Sad mother", alpha = 0.3, colour = "Sad mother")) + 
    geom_point(data = data[data$SAD.x_group == "imputed values",], aes(y = SAD.x_i, x = obs, 
                                                                       shape = "Sad adolescent - imputed", alpha = 0.3, colour = "Sad adolescent - imputed")) + 
    geom_point(data = data[data$SAD.x_group == "known values",], aes(y = SAD.x_i, x = obs, 
                                                                     shape = "Sad adolescent", alpha = 0.3, colour = "Sad adolescent")) + 
    xlab("") + 
    ylab("") + 
    theme_classic() + 
    # guides(colour=guide_legend(title="Affect"), shape = guide_legend(title="")) + 
    # labs(subtitle = "") + 
    scale_x_continuous(breaks = c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56)) + 
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) + 
    geom_vline(xintercept = c(28), linetype = "dashed", color = "grey", size = .3)  + 
    #scale_shape_manual(values=c(8,19)) +  # 20 for circle white filling, 18  
    scale_alpha(guide = "none") + 
    scale_colour_manual(name = "Affect",                               # works alphabetically 
                        labels = c("Sad adolescent", 
                                   "Sad adolescent - imputed",
                                   "Sad father",
                                   "Sad father - imputed", 
                                   "Sad mother",
                                   "Sad mother - imputed"),
                        values = c("#E69F00", "#E69F00", "#56B4E9", "#56B4E9", "#009E73", "#009E73")) + 
    scale_shape_manual(name = "Affect",
                       labels = c("Sad adolescent",
                                  "Sad adolescent - imputed",
                                  "Sad father",
                                  "Sad father - imputed", 
                                  "Sad mother",
                                  "Sad mother - imputed"),
                       values = c(19, 8, 19, 8, 19, 8))
  #theme(legend.position = "none")
  
  return(after_imp2)
  # grid.arrange(after_imp2, legend_after_imp22, ncol = 2, widths=c(3.5,1.5))
  # after_imp_plot[[i]] <- arrangeGrob(after_imp2, legend_after_imp22, ncol = 2, widths=c(5/6,1/6))
}

ema_imp_plotrelaxed3 <- function(data, individuals, familyi){
  
  #### Actual plot 
  ### this works but legend not ideal 
  data <- data[individuals == unique(individuals)[familyi],]
  after_imp2 <- ggplot(data) + 
    geom_line(aes(y = RELAX_i, x = obs, colour = "Relaxed father")) + 
    geom_line(aes(y = RELAX.y_i, x = obs, colour = "Relaxed mother")) + 
    geom_line(aes(y = RELAX.x_i, x = obs, colour = "Relaxed adolescent")) + 
    geom_point(data = data[data$RELAX_group == "imputed values",], aes(y = RELAX_i, x = obs, 
                                                                       shape = "Relaxed father - imputed", alpha = 0.3, colour = "Relaxed father - imputed")) + 
    geom_point(data = data[data$RELAX_group == "known values",], aes(y = RELAX_i, x = obs, 
                                                                     shape = "Relaxed father", alpha = 0.3, colour = "Relaxed father")) + 
    geom_point(data = data[data$RELAX.y_group == "imputed values",], aes(y = RELAX.y_i, x = obs, 
                                                                         shape = "Relaxed mother - imputed", alpha = 0.3, colour = "Relaxed mother - imputed")) + 
    geom_point(data = data[data$RELAX.y_group == "known values",], aes(y = RELAX.y_i, x = obs, 
                                                                       shape = "Relaxed mother", alpha = 0.3, colour = "Relaxed mother")) + 
    geom_point(data = data[data$RELAX.x_group == "imputed values",], aes(y = RELAX.x_i, x = obs, 
                                                                         shape = "Relaxed adolescent - imputed", alpha = 0.3, colour = "Relaxed adolescent - imputed")) + 
    geom_point(data = data[data$RELAX.x_group == "known values",], aes(y = RELAX.x_i, x = obs, 
                                                                       shape = "Relaxed adolescent", alpha = 0.3, colour = "Relaxed adolescent")) + 
    xlab("Observation") + 
    ylab("Score") + 
    theme_classic() + 
    # guides(colour=guide_legend(title="Affect"), shape = guide_legend(title="")) + 
    # labs(subtitle = "") + 
    scale_x_continuous(breaks = c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56)) + 
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) + 
    geom_vline(xintercept = c(28), linetype = "dashed", color = "grey", size = .3)  + 
    #scale_shape_manual(values=c(8,19)) +  # 20 for circle white filling, 18  
    scale_alpha(guide = "none") + 
    scale_colour_manual(name = "Affect",                               # works alphabetically 
                        labels = c("Relaxed adolescent", 
                                   "Relaxed adolescent - imputed",
                                   "Relaxed father",
                                   "Relaxed father - imputed", 
                                   "Relaxed mother",
                                   "Relaxed mother - imputed"),
                        values = c("#E69F00", "#E69F00", "#56B4E9", "#56B4E9", "#009E73", "#009E73")) + 
    scale_shape_manual(name = "Affect",
                       labels = c("Relaxed adolescent",
                                  "Relaxed adolescent - imputed",
                                  "Relaxed father",
                                  "Relaxed father - imputed", 
                                  "Relaxed mother",
                                  "Relaxed mother - imputed"),
                       values = c(19, 8, 19, 8, 19, 8))
  #theme(legend.position = "none")
  
  return(after_imp2)
  # grid.arrange(after_imp2, legend_after_imp22, ncol = 2, widths=c(3.5,1.5))
  # after_imp_plot[[i]] <- arrangeGrob(after_imp2, legend_after_imp22, ncol = 2, widths=c(5/6,1/6))
}

ema_imp_plotirritated3 <- function(data, individuals, familyi){
  
  #### Actual plot 
  ### this works but legend not ideal 
  data <- data[individuals == unique(individuals)[familyi],]
  after_imp2 <- ggplot(data) + 
    geom_line(aes(y = IRRI_i, x = obs, colour =   "father")) + 
    geom_line(aes(y = IRRI.y_i, x = obs, colour = "mother")) + 
    geom_line(aes(y = IRRI.x_i, x = obs, colour = "adolescent")) + 
    geom_point(data = data[data$IRRI_group == "imputed values",], aes(y = IRRI_i, x = obs, 
                                                                      shape = "father - imputed", alpha = 0.3, colour = "father - imputed")) + 
    geom_point(data = data[data$IRRI_group == "known values",], aes(y = IRRI_i, x = obs, 
                                                                    shape = "father", alpha = 0.3, colour = "father")) + 
    geom_point(data = data[data$IRRI.y_group == "imputed values",], aes(y = IRRI.y_i, x = obs, 
                                                                        shape = "mother - imputed", alpha = 0.3, colour = "mother - imputed")) + 
    geom_point(data = data[data$IRRI.y_group == "known values",], aes(y = IRRI.y_i, x = obs, 
                                                                      shape = "mother", alpha = 0.3, colour = "mother")) + 
    geom_point(data = data[data$IRRI.x_group == "imputed values",], aes(y = IRRI.x_i, x = obs, 
                                                                        shape = "adolescent - imputed", alpha = 0.3, colour = "adolescent - imputed")) + 
    geom_point(data = data[data$IRRI.x_group == "known values",], aes(y = IRRI.x_i, x = obs, 
                                                                      shape = "adolescent", alpha = 0.3, colour = "adolescent")) + 
    xlab("Observation") + 
    ylab("") + 
    theme_classic() + 
    # guides(colour=guide_legend(title="Affect"), shape = guide_legend(title="")) + 
    # labs(subtitle = "") + 
    scale_x_continuous(breaks = c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56)) + 
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) + 
    geom_vline(xintercept = c(28), linetype = "dashed", color = "grey", size = .3)  + 
    #scale_shape_manual(values=c(8,19)) +  # 20 for circle white filling, 18  
    scale_alpha(guide = "none") + 
    scale_colour_manual(name = "Affect",                               # works alphabetically 
                        labels = c("adolescent", 
                                   "adolescent - imputed",
                                   "father",
                                   "father - imputed", 
                                   "mother",
                                   "mother - imputed"),
                        values = c("#E69F00", "#E69F00", "#56B4E9", "#56B4E9", "#009E73", "#009E73")) + 
    scale_shape_manual(name = "Affect",
                       labels = c("adolescent",
                                  "adolescent - imputed",
                                  "father",
                                  "father - imputed", 
                                  "mother",
                                  "mother - imputed"),
                       values = c(19, 8, 19, 8, 19, 8))
  #theme(legend.position = "none")
  
  return(after_imp2)
  # grid.arrange(after_imp2, legend_after_imp22, ncol = 2, widths=c(3.5,1.5))
  # after_imp_plot[[i]] <- arrangeGrob(after_imp2, legend_after_imp22, ncol = 2, widths=c(5/6,1/6))
}

# To get family figure all affects 
famfig <- function(famnumb){
  happyfig7 <- ema_imp_plot3(mood_a_new2_imp, mood_a_new2_imp$Family, famnumb) + theme(legend.position = "none") + ggtitle("Happy")
  sadfig7 <- ema_imp_plotsad3(mood_a_new2_imp, mood_a_new2_imp$Family, famnumb) + theme(legend.position = "none") + ggtitle("Sad")
  relaxfig7 <- ema_imp_plotrelaxed3(mood_a_new2_imp, mood_a_new2_imp$Family, famnumb) + theme(legend.position = "none") + ggtitle("Relaxed")
  irrifig7 <- ema_imp_plotirritated3(mood_a_new2_imp, mood_a_new2_imp$Family, famnumb) + ggtitle("Irritated") + theme(legend.title = element_blank())
  legirrifig7 <- get_legend(irrifig7)
  irrifig7 <- irrifig7 + theme(legend.position = "none")
  
  grid.arrange(happyfig7, sadfig7, legirrifig7, relaxfig7, irrifig7, 
               ncol = 3, nrow = 2, widths = c(0.4, 0.4, 0.2), top = textGrob(paste("Family", famnumb), gp=gpar(fontsize=15, font = 2), just = "left", hjust = 6.8)) # plot together 
}

# To get all affect states of a family - # 'addfamilynumber' should be replaced with the family number of the family of interest but unfortunately, the data used in the study is not available online 
famfig('addfamilynumber')

# exported figure in 6x8 
