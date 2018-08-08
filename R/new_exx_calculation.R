## Function to calculate the value of the probability sum exx 
## for the community assortativity calculus

rcomm_exx <- function(data) {
    types <- data$nodeclusters
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
    Pij_edgelist[x] <- F
    
    #Paso 4: Determinar los conjuntos a los que pertenece cada enlace
    edges_subsets <- do.call(rbind, strsplit(gsub("C", "", names(Pij_edgelist)[-c(1:3)]), split = "_"))
    edges_subsets[, 1] <- names(Pij_edgelist)[-c(1:3)]
    
    for(k in 1:nrow(edges_subsets)) {
        for(m in 1:nrow(Pij_edgelist)) {
            i = as.numeric(edges_subsets[k, 2])
            #j = as.numeric(edges_subsets[k, 3])
            
            if(all(Pij_edgelist[m, 1:2] %in% types$node[types$cluster == i])) {
                
                Pij_edgelist[m, names(Pij_edgelist) == edges_subsets[k, 1]] <- TRUE
            }
        }
        print(paste(k, "|", edges_subsets[k, 1]))
    }
    
    #Paso 5: Calcular la probabilidad de Exy^2 por cada intersección posible
    #empezando por los enlaces más intersectados en diferentes comunidades
    Intersections <- sort(unique(rowSums(Pij_edgelist[, -c(1:3)])), decreasing = T)
    
    Exx <- 0
    edges_count <- 0
    for(i in 1:length(Intersections)) {
        Pij_edgelist_split <- Pij_edgelist[rowSums(Pij_edgelist[, -c(1:3)]) == Intersections[i], ]
        
        Pij_split_internames <- apply(Pij_edgelist_split[, -c(1:3)], 1, function(x) paste(edges_subsets[as.logical(x), 1], collapse = " & "))
        
        Internames <- unique(Pij_split_internames)
        
        for(j in 1:length(Internames)) {
            Exx <- Exx + sum(Pij_edgelist_split$Prob_ij[Pij_split_internames == Internames[j]])/sum(2*Pij_edgelist$Prob_ij)
            Exx <- 2*Exx
            edges_count <- edges_count + length(Pij_edgelist_split$Prob_ij[Pij_split_internames == Internames[j]])
        }
    }
}
