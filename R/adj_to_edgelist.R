adj_to_edgelist <- function(x) {
  
  ## 'x' must be an adjacency matrix, where each row and column must have it's own
  ## id or particular name
  
  y <- list()
  
  for (i in 1:nrow(x)) { 
    w <- cbind(1:ncol(x), 1:ncol(x))
    w[, 1] <- rep(dimnames(x)[[1]][i], ncol(x))
    w[, 2] <- dimnames(x)[[2]]
    w <- as.data.frame(w, stringsAsFactors = F)
    for (j in 1:ncol(x)) { w[j, 3] <- x[i, j] }
    y[[i]] <- w
  }
  
  ## Each element from the list 'y' is a matrix 'w' with 3 columns
  ## all with so many rows as the number of columns in 'x'. The first
  ## column contains row 'i' repetitions, the second contains all columns
  ## names, and the third contains the values of those links.
  
  z <- do.call("rbind", y)
  
  ## 'z' is a matrix that contains all possible links. It's the union of
  ## all elements in the 'y' list by row
  
  logicz <- !is.na(z[, 3])
  z1 <- z[logicz,]
  z1
  
    ## 'z1' is a matrix without NA's, given a subset made with the logic vector 'logicz'
  ## to know which rows don't contains NA's
  
}
