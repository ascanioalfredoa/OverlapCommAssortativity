# --- 
# title: "backbone2" 
# author: Alfredo Ascanio
# date: "02/10/2017"
# e-mail: "11-10060@usb.ve"
# --- 

## 'backbone2' function takes and 'x' igraph object and produce a
## network 'backbone' that contains the original nodes names
## and not the number id of that node (which is the default setting of the
## 'backbone' igraph function)

backbone2 <- function(x){

require(igraph)
  
  ## We extract the edge matrix from the graph
edge_matrix <- as_data_frame(x)

  ## Convert the edge_matrix into one vector and assign a number

edge_list <- c(edge_matrix[, 1], edge_matrix[, 2])

edge_list <- unique(edge_list) ## Mantain only unique cases

numeric_edges <- data.frame(edges = as.character(edge_list), number = c(1:length(edge_list)),
                            stringsAsFactors = F)

## Reassign the edge_matrix values with regards to the numeric_edges values

new_edgem <- edge_matrix
for(i in 1:nrow(numeric_edges)) {
    new_edgem[new_edgem == numeric_edges[i, 1]] <- numeric_edges[i, 2]
}

y <- graph.data.frame(new_edgem)
num_backbone <- backbone(y)

char_backbone <- num_backbone
for(i in 1:nrow(numeric_edges)) {
  char_backbone[, 1:2][num_backbone[, 1:2] == numeric_edges[i, 2]] <- numeric_edges[i, 1]
}
return(char_backbone)
}
