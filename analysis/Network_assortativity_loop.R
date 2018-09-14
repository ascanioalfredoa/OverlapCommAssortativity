#### Loaded networks and sample matrices ####
loaded_networks <- data.frame(matrix(ls()[grep("_g|_raw", ls())], ncol = 2, byrow = 2), 
                              stringsAsFactors = F)
names(loaded_networks) <- c("Graph", "RawData")

#### Results matrix ####

Results <- data.frame(array(data = 0, dim = c(nrow(loaded_networks), 3), dimnames = list(NULL, c("Data", "Shizuka", "Ascanio"))))

i <- 1

Results$Data[i] <- gsub("_g", "", loaded_networks$Graph[i])
Results$Shizuka

calc_rc(data = eval(parse(text = loaded_networks$RawData[i])), 
        n.bootstraps = 100, gbi.format = F)


