############################################################################################
#
# Script to calculate r_c, from Shizuka & Farine
#
# This is an example script for calculating community assortativity (r_c) from empirical data. 
# We use the tit dataset available through the asnipe package. **Note that the figure in the 
# publication excludes a single isolate node for clarity. 
# We assume that the data is in a "group-by-individual" format, where groups appear in rows,
# individuals appear in columns, and cell value is 1 if the individual is seen in the group 
# and 0 otherwise. A 'group' here is defined as a set of individuals observed in close proximity
# during a given observation data point. 
#
############################################################################################

# Load required libraries
library(igraph)
library(asnipe)
library(assortnet)


#load & check dataset. This will print the first 10 rows of the data, called 'gbi'
#data("group_by_individual")
#head(gbi) 

#Function to calculate r_c, with default number of bootstraps = 100, and default option to plot result. Plot will be saved as pdf file in R output folder as "rc_result.pdf".

calc_rc=function(data, n.bootstraps=100, plot.result=T){

# Create space to store results from bootstraps
network.community <- matrix(0,ncol(data),ncol(data))
network.present <- matrix(0,ncol(data),ncol(data))
	
# 1. Calculate network
network <- get_network(data,data_format="GBI", association_index="SRI")
	
# 2. Calculate community membership of the observed network
community.observed <- fastgreedy.community(graph.adjacency(network,mode="undirected",weighted=TRUE))

# 3. Main bootstrapping method: i) Bootstrap the observed data, ii) recalculate the network, 
#    iii) recalculate community membership, iv) check if both individuals are observed

for (i in 1:n.bootstraps) {
		# This step bootrstraps the sampling periods
		gbi.boot <- data[sample(1:nrow(data),nrow(data),replace=TRUE),]
		network.boot <- get_network(gbi.boot,data_format="GBI", association_index="SRI")
		
		# This step calculates the community membership from the bootstrapped network
		community.boot <- fastgreedy.community(graph.adjacency(network.boot,mode="undirected",weighted=TRUE))

		# This step adds 1 to any dyads in the same community
		network.community <- network.community + outer(community.boot$membership, community.boot$membership,"==")


		# This step adds 1 to any dyads that are both present (in this case if they have at least 1 edge)
		network.present <- network.present + outer((rowSums(network.boot)>0),(rowSums(network.boot)>0),"*")
	}
# End bootstrap

# Calculate proportion of times observed in the same community
P <- network.community/network.present
P[!is.finite(P)] <- 0
	
# Calculate assortment from known community membership
rc <- assortment.discrete(P,community.observed$membership)$r

#if the argument plot.result=T, then generate plot network of probabilities that nodes are assigned to the same community in bootstraps. It will be saved as pdf file called "rc_result.pdf" in your R output folder
	if(plot.result) {
		pdf("rc_result.pdf")
		diag(P)=0
		g=graph.adjacency(P, "undirected", weighted=T)
		plot(g, edge.width=E(g)$weight, vertex.label="", vertex.size=5, vertex.color=membership(community.observed))
		dev.off()
		}
	
return(rc)
}
#end function

# run the function for the attached dataset, called "gbi". Here, we reduce the number of bootstraps to shorten the run time for demonstration. 

#calc_rc(gbi, n.bootstraps=10, plot.result=T)




