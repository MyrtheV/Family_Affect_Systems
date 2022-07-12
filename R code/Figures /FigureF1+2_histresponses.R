# Figure distribution responses 

# Load packages 
library(ggplot2)

# Unfortunately, we are unable to share the data  

happyhistdata <- data.frame(value = c(mood_a_new2_imp_22102021$HAPPY.x,   # responses (data)
                                      mood_a_new2_imp_22102021$HAPPY.y, 
                                      mood_a_new2_imp_22102021$HAPPY), 
                            group = c(rep("Adolescent", length(mood_a_new2_imp_22102021$HAPPY.x)), 
                                      rep("Mother", length(mood_a_new2_imp_22102021$HAPPY.y)), 
                                      rep("Father", length(mood_a_new2_imp_22102021$HAPPY))))

happyhistdata <- data.frame(value = c(mood_a_new2_imp_22102021$HAPPY.x,  # responses (data)
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

# Per family  
plotfam_list <- list()
for(i in 1:length(unique(mood_a_new2_imp_22102021$Family))){
  
  mood_a_new2_imp_22102021_fam <- mood_a_new2_imp_22102021[mood_a_new2_imp_22102021$Family == unique(mood_a_new2_imp_22102021$Family)[i], ]
 
  happyhistdata_fam <- data.frame(value = c(mood_a_new2_imp_22102021_fam$HAPPY.x,   # responses (data)
                                        mood_a_new2_imp_22102021_fam$HAPPY.y, 
                                        mood_a_new2_imp_22102021_fam$HAPPY), 
                              group = c(rep("Adolescent", length(mood_a_new2_imp_22102021_fam$HAPPY.x)), 
                                        rep("Mother", length(mood_a_new2_imp_22102021_fam$HAPPY.y)), 
                                        rep("Father", length(mood_a_new2_imp_22102021_fam$HAPPY))))
  
  happyhistdata_fam <- data.frame(value = c(mood_a_new2_imp_22102021_fam$HAPPY.x,  # responses (data)
                                        mood_a_new2_imp_22102021_fam$HAPPY.y, 
                                        mood_a_new2_imp_22102021_fam$HAPPY, 
                                        mood_a_new2_imp_22102021_fam$SAD.x, 
                                        mood_a_new2_imp_22102021_fam$SAD.y, 
                                        mood_a_new2_imp_22102021_fam$SAD, 
                                        mood_a_new2_imp_22102021_fam$RELAX.x, 
                                        mood_a_new2_imp_22102021_fam$RELAX.y, 
                                        mood_a_new2_imp_22102021_fam$RELAX, 
                                        mood_a_new2_imp_22102021_fam$IRRI.x, 
                                        mood_a_new2_imp_22102021_fam$IRRI.y, 
                                        mood_a_new2_imp_22102021_fam$IRRI), 
                              group = rep(c(rep("Adolescent", length(mood_a_new2_imp_22102021_fam$HAPPY.x)), 
                                            rep("Mother", length(mood_a_new2_imp_22102021_fam$HAPPY.y)), 
                                            rep("Father", length(mood_a_new2_imp_22102021_fam$HAPPY))),4), 
                              variable = c(rep("Happy", 168), rep ("Sad", 168), 
                                           rep("Relaxed", 168), rep("Irritated", 168)))
  
  histdata_fam_fig <- ggplot(happyhistdata_fam, aes(x = value, fill = group)) +  
    geom_histogram(position = "identity") + 
    scale_fill_manual(values = (c("#E69F00", "#56B4E9", "#009E73"))) + 
    facet_grid(group ~ variable) + 
    xlab("Response") + 
    ylab("Count") + 
    theme(legend.position = "None") + 
    scale_x_continuous(breaks = 1:7) + 
    ggtitle(element_text(paste("Family", i)))
  
  plotfam_list[[i]] <- histdata_fam_fig
}

pdf("histogram_resp_family.pdf", onefile = TRUE)
print(plotfam_list)
dev.off()





