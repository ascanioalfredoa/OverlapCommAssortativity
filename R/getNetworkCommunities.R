getNetworkCommunities <- function(data, algorithm = "linkcomm") {
    
    if (!(algorithm %in% c("linkcomm", "fastgreedy", "edge_betweennes",
                           "walktrap", "spinglass", "leading_eigenvector"))) {
        stop("The algorithm requested is not taken into account by the function")
    }
    
    if(algorithm == "linkcomm") {
        # 1. Generate adjacency matrix
        network <- as.matrix(as_adj(graph, type = "lower", attr = "weight"))
        network <- adj_to_edgelist(network)
        network <- network[network$V3 != 0, ]
        # 2. Calculate community membership of the observed network
        community.observed <- getLinkCommunities(network, plot = F)
        if(length(getAllNestedComm(community.observed)) != 0) {
            community.observed <- newLinkCommsAt(ff, cutat = getMaxden(ff)[1]) # Linked communities without overlaping
        } 
        community.observed <- community.observed$nodeclusters
        
    } else if (algorithm == "fastgreedy") {
        community.observed <- fastgreedy.community(graph)
        community.observed <- data.frame(node = as.character(community.observed$names), cluster = as.numeric(community.observed$membership), stringsAsFactors = F)
    } else if (algorithm == "edgebetweennes") {
        community.observed <- edge.betweenness.community(graph)
        community.observed <- data.frame(node = as.character(community.observed$names), cluster = as.numeric(community.observed$membership), stringsAsFactors = F)
    } else if (algorithm == "walktrap") {
        community.observed <- walktrap.community(graph)
        community.observed <- data.frame(node = as.character(community.observed$names), cluster = as.numeric(community.observed$membership), stringsAsFactors = F)
    } else if (algorithm == "spinglass") {
        community.observed <- spinglass.community(graph)
        community.observed <- data.frame(node = as.character(community.observed$names), cluster = as.numeric(community.observed$membership), stringsAsFactors = F)
    } else { #(algorithm = "leading_eigenvector")
        community.observed <- leading.eigenvector.community(graph)
        community.observed <- data.frame(node = as.character(community.observed$names), cluster = as.numeric(community.observed$membership), stringsAsFactors = F)
    }
    
    return(community.observed)
}
