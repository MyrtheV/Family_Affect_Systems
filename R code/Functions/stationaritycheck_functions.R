# Stationarity Check Functions 
library(tseries)

stationarity_check <- function(dataset){
 
   variables <- c("Family", "HAPPY.x", "SAD.x", "RELAX.x", "IRRI.x", 
                 "HAPPY.y", "SAD.y", "RELAX.y", "IRRI.y", 
                 "HAPPY", "SAD", "RELAX", "IRRI")
  
  dataset <- dataset[, c(which(colnames(dataset) %in% variables))]
  kpps_p <- matrix(NA, nrow = length(unique(dataset$Family)), ncol = ncol(dataset))
  # variable <- 1 
  kpps_p[,1] <- unique(dataset$Family)
  for(j in 2:ncol(dataset)){
    # variable <- variable + 1 
    for(i in 1:length(unique(dataset$Family)))
    {
      randomness <- tseries::kpss.test(na.exclude(dataset[, j][dataset$Family == unique(dataset$Family)[i]]), lshort = TRUE, null = "Level")
      kpps_p[i,j] <- randomness$p.value
    }

  }
  kpps_p_corrected <- 0.05/length(unique(dataset$Family))
  which(kpps_p < kpps_p_corrected)
  colnames(kpps_p) <- variables[which(colnames(dataset) %in% variables)]
  
  kpps_p_result <- list()
  kpps_p_result$p <- kpps_p
  kpps_p_result$sig <- which(kpps_p < kpps_p_corrected)
  
  return(kpps_p_result)
}


stationarity_check2 <- function(dataset){
  
  variables <- c("Family", "HAPPY.x", "SAD.x", "RELAX.x", "IRRI.x", 
                 "HAPPY.y", "SAD.y", "RELAX.y", "IRRI.y", 
                 "HAPPY", "SAD", "RELAX", "IRRI")
  
  dataset <- dataset[, c(which(colnames(dataset) %in% variables))]
  kpps_p <- matrix(NA, nrow = length(unique(dataset$Family)), ncol = ncol(dataset))
   
  kpps_p[,1] <- unique(dataset$Family)
  for(j in 2:ncol(dataset)){
    
    for(i in 1:length(unique(dataset$Family)))
    {
      randomness <- tseries::kpss.test(na.exclude(dataset[, j][dataset$Family == unique(dataset$Family)[i]]), lshort = TRUE, null = "Trend")
      kpps_p[i,j] <- randomness$p.value
    }
  }
  kpps_p_corrected <- 0.05/length(unique(dataset$Family))
  which(kpps_p < kpps_p_corrected)
  colnames(kpps_p) <- variables[which(colnames(dataset) %in% variables)]
  
  kpps_p_result <- list()
  kpps_p_result$p <- kpps_p
  kpps_p_result$sig <- which(kpps_p < kpps_p_corrected)
  
  return(kpps_p_result)
}

###################
# Function for after imputation 

stationarityimp_check <- function(dataset){
  variables <- c("Family", "HAPPY.x_i", "SAD.x_i", "RELAX.x_i", "IRRI.x_i", 
                 "HAPPY.y_i", "SAD.y_i", "RELAX.y_i", "IRRI.y_i", 
                 "HAPPY_i", "SAD_i", "RELAX_i", "IRRI_i")
  
  dataset <- dataset[, c(which(colnames(dataset) %in% variables))]
  kpps_p <- matrix(NA, nrow = length(unique(dataset$Family)), ncol = ncol(dataset))
  
  kpps_p[,1] <- unique(dataset$Family)
  for(j in 2:ncol(dataset)){
    
    for(i in 1:length(unique(dataset$Family)))
    {
      randomness <- tseries::kpss.test(na.exclude(dataset[, j][dataset$Family == unique(dataset$Family)[i]]), lshort = TRUE, null = "Level")
      kpps_p[i,j] <- randomness$p.value
    }
  }
  kpps_p_corrected <- 0.05/length(unique(dataset$Family))
  which(kpps_p < kpps_p_corrected)
  colnames(kpps_p) <- variables[which(colnames(dataset) %in% variables)]
  
  kpps_p_result <- list()
  kpps_p_result$p <- kpps_p
  kpps_p_result$sig <- which(kpps_p < kpps_p_corrected)
  
  return(kpps_p_result)
}

stationarityimp_check2 <- function(dataset){
  
  variables <- c("Family", "HAPPY.x_i", "SAD.x_i", "RELAX.x_i", "IRRI.x_i", 
                 "HAPPY.y_i", "SAD.y_i", "RELAX.y_i", "IRRI.y_i", 
                 "HAPPY_i", "SAD_i", "RELAX_i", "IRRI_i")
  
  dataset <- dataset[, c(which(colnames(dataset) %in% variables))]
  kpps_p <- matrix(NA, nrow = length(unique(dataset$Family)), ncol = ncol(dataset))
   
  kpps_p[,1] <- unique(dataset$Family)
  for(j in 2:ncol(dataset)){
    
    for(i in 1:length(unique(dataset$Family)))
    {
      randomness <- tseries::kpss.test(na.exclude(dataset[, j][dataset$Family == unique(dataset$Family)[i]]), lshort = TRUE, null = "Trend")
      kpps_p[i,j] <- randomness$p.value
    }
    
  }
  kpps_p_corrected <- 0.05/length(unique(dataset$Family))
  which(kpps_p < kpps_p_corrected)
  colnames(kpps_p) <- variables[which(colnames(dataset) %in% variables)]
  
  kpps_p_result <- list()
  kpps_p_result$p <- kpps_p
  kpps_p_result$sig <- which(kpps_p < kpps_p_corrected)
  
  return(kpps_p_result)
}


