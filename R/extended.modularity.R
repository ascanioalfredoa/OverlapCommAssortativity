extended.modularity <- function(grouping, graph, initial.community){
  m <- ecount(graph)
  mat <- as_adjacency_matrix(graph)
  leg <- length(grouping$labels)
  EQ.cut <- rep(0, times=leg)
  # evaluate every potential partition
  for (cut in 1:leg){
    cutting <- as.numeric(cutree(grouping, k=cut))
    EQ.Ci <- 0
    belongs <- rep(0, times=vcount(graph))
    # determine overlapping nodes
    for (Ci in 1:max(cutting)){
      cliques <- which(cutting==Ci)
      all.nodes.Ci <- as.numeric(unlist(initial.community[cliques]))
      all.nodes.Ci <- sort(unique(all.nodes.Ci))
      belongs[all.nodes.Ci] <- belongs[all.nodes.Ci]+1
    }
    # evaluate every community of the partition
    for (Ci in 1:max(cutting)){
      cliques <- which(cutting==Ci)
      all.nodes.Ci <- as.numeric(unlist(initial.community[cliques]))
      all.nodes.Ci <- sort(unique(all.nodes.Ci))
      EQ.local <- matrix(0, nrow=length(all.nodes.Ci), ncol=length(all.nodes.Ci))
      # evaluate every pair of nodes in the community
      for (i in 1:length(all.nodes.Ci)){
        for (j in 1:length(all.nodes.Ci)){
          v <- all.nodes.Ci[i]
          w <- all.nodes.Ci[j]
          overlap <- 1/(belongs[v]*belongs[w])
          EQ.local[i,j] <- overlap*(mat[v,w]-((sum(mat[v,])*sum(mat[w,]))/(2*m)))
        }
      }
      EQ.Ci[Ci] <- sum(EQ.local)
    }
    EQ.cut[cut] <- (sum(EQ.Ci))/(2*m)
  }
  return(EQ.cut)
}
