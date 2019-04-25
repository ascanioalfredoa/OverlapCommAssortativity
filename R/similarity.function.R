similarity_function <- function(community, graph){
  le.com <- length(community)
  similarity.matrix <- matrix(0, nrow=le.com, ncol=le.com)
  m <- ecount(graph)
  mat <- as_adjacency_matrix(graph)
  # accross the community list
  for (i in 1:le.com){
    for (j in 1:le.com){
      # accros the elements of each community
      n <- 1
      sumatory <- 0
      for (x in 1:length(community[[i]]) ){
        for (y in 1:length(community[[j]])){
          v <- community[[i]][x]
          w <- community[[j]][y]
          if (v != w){
            sumatory[n] <- mat[v,w]-((sum(mat[v,])*sum(mat[w,]))/(2*m))
            n <- n+1
          }
        }
      }
      similarity.matrix[i,j] <- sum(sumatory)/(2*m)
    }
  }
  diag(similarity.matrix) <- NA
  similarity.matrix[lower.tri(similarity.matrix)] <- NA
  return(similarity.matrix)
}
