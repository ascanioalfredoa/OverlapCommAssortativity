rcomm_exy <- function(data) {
  #Paso 1: Matriz de pertenencia de nodos a comunidades
  comm_list <- data$nodeclusters
  
  #Paso 2: Matriz de enlaces basada en Pij
  Pij_edgelist <- data$Pij
  
  Pij_edgelist[upper.tri(Pij_edgelist)] <- F
  
  Pij_edgelist <- cbind(Pij_edgelist, Topic = row.names(Pij_edgelist))
  
  Pij_edgelist <- melt(Pij_edgelist, variable.name = "Topic", value.name = "Prob_ij")
  
  Pij_edgelist <- Pij_edgelist[Pij_edgelist[, 3] != 0, ]
  
  names(Pij_edgelist)[2] <- "Topic2"
  
  Pij_edgelist[, 1:2] <- sapply(Pij_edgelist[, 1:2], as.character)
  Pij_edgelist[, 3] <- as.numeric(as.character(Pij_edgelist[, 3]))
  
  Pij_edgelist <- Pij_edgelist[!is.na(Pij_edgelist[, 3]), ]
  
  #Paso 3: Ampliar matriz Pij_edgelist
  Pij_edgelist[xy_subsets(data)] <- F
  
  #Paso 4: Determinar los conjuntos a los que pertenece cada enlace
  edges_subsets <- do.call(rbind, strsplit(gsub("C", "", names(Pij_edgelist)[-c(1:3)]), split = "_"))
  edges_subsets[, 1] <- names(Pij_edgelist)[-c(1:3)]
  
  for(k in 1:nrow(edges_subsets)) {
    for(m in 1:nrow(Pij_edgelist)) {
      i = as.numeric(edges_subsets[k, 2])
      j = as.numeric(edges_subsets[k, 3])
      
      if(Pij_edgelist[m, 1] %in% comm_list$node[comm_list$cluster == i] & Pij_edgelist[m, 2] %in% comm_list$node[comm_list$cluster == j] |
         Pij_edgelist[m, 1] %in% comm_list$node[comm_list$cluster == j] & Pij_edgelist[m, 2] %in% comm_list$node[comm_list$cluster == i]) {
        
        Pij_edgelist[m, names(Pij_edgelist) == edges_subsets[k, 1]] <- TRUE
      }
    }
    print(paste(k, "|", edges_subsets[k, 1]))
  }
  
  #Paso 5: Calcular la probabilidad de Exy^2 por cada intersección posible
  #empezando por los enlaces más intersectados en diferentes comunidades
  if(ncol(Pij_edgelist) == 4) {
      Exy <- (sum(Pij_edgelist$Prob_ij)^2)/((sum(2*Pij_edgelist$Prob_ij))^2)
  } else {
      Intersections <- sort(unique(rowSums(Pij_edgelist[, -c(1:3)])), decreasing = T)
      
      Exy <- 0
      edges_count <- 0
      for(i in 1:length(Intersections)) {
          Pij_edgelist_split <- Pij_edgelist[rowSums(Pij_edgelist[, -c(1:3)]) == Intersections[i], ]
          
          Pij_split_internames <- apply(Pij_edgelist_split[, -c(1:3)], 1, function(x) paste(edges_subsets[as.logical(x), 1], collapse = " & "))
          
          Internames <- unique(Pij_split_internames)
          
          for(j in 1:length(Internames)) {
              Exy <- Exy + sum(Pij_edgelist_split$Prob_ij[Pij_split_internames == Internames[j]])#/sum(2*Pij_edgelist$Prob_ij)^2
              edges_count <- edges_count + length(Pij_edgelist_split$Prob_ij[Pij_split_internames == Internames[j]])
          }
      }
      
      Exy <- Exy/((2*sum(Pij_edgelist$Prob_ij))^2)
  }
  
  #Nodos que quedan fuera del análisis
  #names(V(coral.backbone))[!names(V(coral.backbone)) %in% as.character(unique(comm_list$node))]
  
  #Enlaces que quedan sin asignar
  #sum(rowSums(Pij_edgelist[, -c(1:3)]) == 0)
  return(Exy)
}
