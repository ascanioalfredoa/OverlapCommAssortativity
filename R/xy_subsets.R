xy_subsets <- function(data) {
  types <- data$comm_obs$nodeclusters
  P <- data$Pij
  
  #### Matrices ####
  
  xy <- NULL
  yx <- NULL
  k = 1
  for(i in 1:length(unique(types$cluster))) {
    for(j in 1:length(unique(types$cluster))) {
      
      if(i >= 10) {
        ni <- i
      } else {
        ni <- paste(0, i, sep = "")
      }
      
      if(j >= 10) {
        nj <- j
      } else {
        nj <- paste(0, j, sep = "")
      }
      
      if(paste(ni, nj, sep = "_") %in% yx) {next}
      
      xy[k] <- paste("C", ni, nj, sep = "_")
      yx[k] <- paste(nj, ni, sep = "_")
      k = k + 1
    }
  }
  return(xy)
}
