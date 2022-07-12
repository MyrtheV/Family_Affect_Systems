# Function used for Figure 1 

# Load packages 
library(mlVAR)
library(qgraph)
library(ggplot2)

famvargeneff <- function(model){
  moodamfs_model <- model
  
  # Temporal 
  # Check which general effects are not zero
  nonzerogentemp <- which(getNet(moodamfs_model, "temporal", nonsig = "hide") != 0)
  
  # retrieve the values per family for non zero general effects 
  nonzerofamtemp <- c()
  for(i in 1:length(moodamfs_model$IDs)){
    nonzerofamtemp <- c(nonzerofamtemp, t(moodamfs_model$results$Beta$subject[[i]][,,1])[nonzerogentemp])
  }
  
  # Create dataframe with family effects 
  numbfam <- length(moodamfs_model$IDs)
  tempeffnumb <- rep(1:length(nonzerogentemp), numbfam) 
  numbfamvar <- rep(1:numbfam, each = length(nonzerogentemp))
  tempfameff <- data.frame(family = numbfamvar, edge = tempeffnumb, weight = nonzerofamtemp)
  
  # Create dataframe of general effects 
  tempgeneff <- data.frame(family = rep("General", length(nonzerogentemp)),
                           edge = 1:length(nonzerogentemp), 
                           weight = getNet(moodamfs_model, "temporal", nonsig = "hide")[nonzerogentemp])
  
  # Get the name of the edges in general effects 
  rownamek <- c()
  colnamek <- c()
  k <- 0
  for(i in nonzerogentemp){
    k <- k + 1 
    getindexk <- arrayInd(i, dim(getNet(moodamfs_model, "temporal", nonsig = "hide")))
    rownamek[k] <- rownames(getNet(moodamfs_model, "temporal", nonsig = "hide"))[getindexk[,1]]
    colnamek[k] <- colnames(getNet(moodamfs_model, "temporal", nonsig = "hide"))[getindexk[,2]]
  }
  
  rownamek <- gsub("x_i", "adolescent", rownamek)
  rownamek <- gsub("y_i", "mother", rownamek)
  rownamek <- gsub("_i", ".father", rownamek)
  
  colnamek <- gsub("x_i", "adolescent", colnamek)
  colnamek <- gsub("y_i", "mother", colnamek)
  colnamek <- gsub("_i", ".father", colnamek)
  
  nodenamesedge <- data.frame(outgoingnode = rownamek, incomingnode = colnamek)
  edgenames <- paste(nodenamesedge$outgoingnode, "to", nodenamesedge$incomingnode)
  
  tempfameff$edgenames <- rep(edgenames, numbfam)
  tempgeneff$edgenames <- edgenames
  
  # Contemporaneous - have to look at lower triangle only 
  ## Check which general effects are not zero
  lowertrianglecon <- getNet(moodamfs_model, "contemporaneous", nonsig = "hide", rule = "and")[which(lower.tri(getNet(moodamfs_model, "contemporaneous", nonsig = "hide", rule = "and")))]
  nonzerogencon <- which(lowertrianglecon != 0)
  
  # retrieve the values per family for non zero general effects 
  nonzerofamcon <- c()
  for(i in 1:length(moodamfs_model$IDs)){
    lowertrianglefamcon <- moodamfs_model$results$Theta$pcor$subject[[i]][which(lower.tri(moodamfs_model$results$Theta$pcor$subject[[i]]))]
    nonzerofamcon <- c(nonzerofamcon, lowertrianglefamcon[nonzerogencon])
  }
  
  # Create dataframe with family effects 
  coneffnumb <- rep(1:length(nonzerogencon), numbfam) 
  numbfamvar2 <- rep(1:numbfam, each = length(nonzerogencon))
  confameff <- data.frame(family = numbfamvar2, edge = coneffnumb, weight = nonzerofamcon)
  
  # Create dataframe of general effects 
  congeneff <- data.frame(family = rep("General", length(nonzerogencon)),
                          edge = 1:length(nonzerogencon), 
                          weight = lowertrianglecon[nonzerogencon])
  
  # Get the name of the edges in general effects - continue working here 
  nonzerogencon2 <-  which(getNet(moodamfs_model, "contemporaneous", nonsig = "hide", rule = "and")!=0 & lower.tri(getNet(moodamfs_model, "contemporaneous", nonsig = "hide", rule = "and")))
  rownamekcon <- c()
  colnamekcon <- c()
  k <- 0
  for(i in nonzerogencon2){   
    k <- k + 1 
    getindexk <- arrayInd(i, dim(getNet(moodamfs_model, "contemporaneous", nonsig = "hide", rule = "and")))
    rownamekcon[k] <- rownames(getNet(moodamfs_model, "contemporaneous", nonsig = "hide", rule = "and"))[getindexk[,1]]
    colnamekcon[k] <- colnames(getNet(moodamfs_model, "contemporaneous", nonsig = "hide", rule = "and"))[getindexk[,2]]
  }
  
  rownamekcon <- gsub("x_i", "adolescent", rownamekcon)
  rownamekcon <- gsub("y_i", "mother", rownamekcon)
  rownamekcon <- gsub("_i", ".father", rownamekcon)
  
  colnamekcon <- gsub("x_i", "adolescent", colnamekcon)
  colnamekcon <- gsub("y_i", "mother", colnamekcon)
  colnamekcon <- gsub("_i", ".father", colnamekcon)
  
  nodenamesedgecon <- data.frame(outgoingnode = rownamekcon, incomingnode = colnamekcon)
  edgenamescon <- paste(nodenamesedgecon$outgoingnode, "and", nodenamesedgecon$incomingnode)
  
  confameff$edgenames <- rep(edgenamescon, numbfam)
  congeneff$edgenames <- edgenamescon
  
  efffig <- list(tempfameff = tempfameff, tempgeneff = tempgeneff, 
                 confameff = confameff, congeneff = congeneff) 
  
  # Figure temporal effects 
  ggplot(efffig$tempfameff, mapping = aes(x = as.factor(edge), y = weight, color = edgenames)) + 
    geom_quasirandom(alpha = 0.2, width = 0.2, dodge.width=1) + 
    geom_hline(yintercept = 0, color = "grey", linetype = 2) + 
    #geom_point(tempfameff, mapping = aes(x = as.factor(edge), y = weight, color = as.factor(edge)), alpha = .2) + 
    geom_boxplot(efffig$tempfameff, mapping = aes(x = as.factor(edge), y = weight, color = edgenames, fill = edgenames), alpha = .1) + 
    geom_point(efffig$tempgeneff, mapping = aes(x = as.factor(edge), y = weight), color = "black") + 
    theme_classic() + 
    theme(legend.position = "none") + 
    labs(x = "Edge", y = "Weight")
  
  # Figure contemporaneous effects 
  ggplot(efffig$confameff, mapping = aes(x = as.factor(edge), y = weight, color = edgenames)) + 
    geom_quasirandom(alpha = 0.2, width = 0.2, dodge.width=1) + 
    geom_hline(yintercept = 0, color = "grey", linetype = 2) + 
    #geom_point(tempfameff, mapping = aes(x = as.factor(edge), y = weight, color = as.factor(edge)), alpha = .2) + 
    geom_boxplot(efffig$confameff, mapping = aes(x = as.factor(edge), y = weight, color = edgenames, fill = edgenames), alpha = .1) + 
    geom_point(efffig$congeneff, mapping = aes(x = as.factor(edge), y = weight), color = "black") + 
    theme_classic() + 
    theme(legend.position = "none") + 
    labs(x = "Edge", y = "Weight")
  
  return(efffig)
  
}


# Create figures by hand 
# efffig <- famvargeneff(moodamfs_model)
# 
# tempfamgeneffect <- ggplot(efffig$tempfameff, mapping = aes(x = as.factor(edge), y = weight, color = edgenames)) + 
#   geom_quasirandom(alpha = 0.2, width = 0.2, dodge.width=1) + 
#   geom_hline(yintercept = 0, color = "grey", linetype = 2) + 
#   #geom_point(tempfameff, mapping = aes(x = as.factor(edge), y = weight, color = as.factor(edge)), alpha = .2) + 
#   geom_boxplot(efffig$tempfameff, mapping = aes(x = as.factor(edge), y = weight, color = edgenames, fill = edgenames), alpha = .1) + 
#   geom_point(efffig$tempgeneff, mapping = aes(x = as.factor(edge), y = weight), color = "black") + 
#   theme_classic() + 
#   theme(legend.position = "none") + 
#   labs(x = "Edge", y = "Weight")
# 
# # Figure contemporaneous 
# contempfamgeneffect <- ggplot(efffig$confameff, mapping = aes(x = as.factor(edge), y = weight, color = edgenames)) + 
#   geom_quasirandom(alpha = 0.2, width = 0.2, dodge.width=1) + 
#   geom_hline(yintercept = 0, color = "grey", linetype = 2) + 
#   #geom_point(tempfameff, mapping = aes(x = as.factor(edge), y = weight, color = as.factor(edge)), alpha = .2) + 
#   geom_boxplot(efffig$confameff, mapping = aes(x = as.factor(edge), y = weight, color = edgenames, fill = edgenames), alpha = .1) + 
#   geom_point(efffig$congeneff, mapping = aes(x = as.factor(edge), y = weight), color = "black") + 
#   theme_classic() + 
#   theme(legend.position = "none") + 
#   labs(x = "Edge", y = "Weight")
