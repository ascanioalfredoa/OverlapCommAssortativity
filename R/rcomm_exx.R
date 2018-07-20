## Function to calculate the value of the probability sum exx 
## for the community assortativity calculus

rcomm_exx <- function(data) {
  types <- data$comm_obs$nodeclusters
  P <- data$Pij
  
  #### Combinatoria para termino Exx ####
  x <- NULL # X vector will contain the clusters names
  for(i in 1:length(unique(types$cluster))) {
    
    if(i >= 10)  {
      x[i] <- paste("C", i, sep = "_") #Assign name to cluster i
    } else {
      x[i] <- paste("C_0", i, sep = "") #Assign name to cluster i
    }
    
    
    #Assign to the object C_i a logical matrix with the same dimensions as Pij,
    #which values are TRUE if the edge exists
    #which valuesa re FALSE if the edge doesn't exists
    assign(x[i], 
           outer(rownames(P) %in% types$node[types$cluster == i],
                 colnames(P) %in% types$node[types$cluster == i],
                 "&"))
    print(sum(get(x[i]))) #Number of edges 
  }
  
  
  colnames(P[which(rownames(P) %in% types$node[types$cluster == 11]),
             which(colnames(P) %in% types$node[types$cluster == 11])])
  
  y <- data.frame(array(data = as.character(0), dim = c(0, 2), dimnames = list(c(), c("n", "Combination"))))
  for(i in 1:length(unique(types$cluster))) {
    #Combinations of clusters (possible intersections of k = i groups)
    y <- rbind(y, cbind(n = i, Combination = apply(combn(x, i), 2, FUN = paste, collapse=" & ")))
  }
  
  y$Prob <- 0
  k <- 1
  l <- 1
  repeat {
    Inters <- eval(parse(text = as.character(y[k, 2])))
    if(sum(Inters) > 0) {
      y[k, 3] <- sum(P[Inters])/sum(P)
      k = k + 1
    } else {
      z <- gsub(" ", "", paste("(?=.*", 
                               unlist(strsplit(as.character(y[k, 2]), split = " & ")), 
                               ")", 
                               collapse = ""))
      y <- y[-grep(z, as.character(y[, 2]), perl = T), ]
    }
    l = l + 1
    print(c(Interseccion = l, k, Filas = nrow(y)))
    if(k > nrow(y)) {break}
  }
  
  y$Prob <- ifelse(as.numeric(y$n) %% 2 == 0, -1*y$Prob, y$Prob)
  y <- y[y$Prob != 0, ]
  
  exx <- sum(y$Prob)
  
  return(exx)
}
