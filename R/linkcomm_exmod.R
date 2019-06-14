linkcomm.exmod <- function(linkcomm.object){
    
  require(cba)
  grouping <- list()
  grouping$merge <- data.frame(0)
  grouping$height <- 0
  grouping$order <- 0
  grouping$labels <- list()

  for(i in 1:length(linkcomm.object$clusters)) {
    grouping$labels[[i]] <- (getNodesIn(linkcomm.object, i))
  }

  initial.community <- list()
  for(i in 1:length(grouping$labels)) {
    initial.community[[i]] <- which(V(linkcomm.object$igraph)$name %in% grouping$labels[[i]])
  }

  names(initial.community) <- -1:-length(initial.community)

  similarity_matrix <- similarity_function(initial.community, linkcomm.object$igraph)

  initial.similarity <- similarity_matrix

  community <- initial.community
  names(community) <- -1:-length(grouping$labels)

  merge <- matrix(0, nrow=length(community)-1, ncol=2)
  n <- 1
  while (length(community) != 1){
    # select leaves to merge
    max_sim <- max(similarity_matrix, na.rm=TRUE)
    to.merge <- which(similarity_matrix==max_sim, arr.ind=TRUE)
    x <- community[[to.merge[1,1]]]
    y <- community[[to.merge[1,2]]]
    # save merge
    merge[n,1] <- names(community[to.merge[1,1]])
    merge[n,2] <- names(community[to.merge[1,2]])
    # merge into 1st community, re-name, and delete 2nd
    community[[to.merge[1,1]]] <- union(x,y)
    names(community)[to.merge[1,1]] <- as.character(n)
    community[[to.merge[1,2]]] <- NULL
    # calculate new similar
    similarity_matrix <- similarity_function(community, linkcomm.object$igraph)
    # move forward
    #partition.list[[length(community)]] <- community
    n <- n+1 
  }

  class(grouping) <- "hclust"
  class(merge) <- "integer"
  grouping$merge <- merge
  grouping$height <- 1:(length(grouping$labels)-1)
  grouping$order <- 0

  initial.similarity[is.na(initial.similarity)] <- 0
  initial.similarity <- initial.similarity + t(initial.similarity)
  diag(initial.similarity) <- 1
  dist.mat <- dist(initial.similarity, method="euclidean")

  ordering <- order.optimal(dist.mat, merge)$order
  grouping$order <- as.numeric(ordering)

  extended.modularity(grouping, linkcomm.object$igraph, initial.community)
}
