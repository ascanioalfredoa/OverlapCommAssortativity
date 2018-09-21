#### Loaded networks and sample matrices ####
loaded_networks <- data.frame(matrix(ls()[grep("_g|_raw", ls())], ncol = 2, byrow = 2), 
                              stringsAsFactors = F)
names(loaded_networks) <- c("Graph", "RawData")
loaded_networks$gbi.format <- c(rep(FALSE, 4), rep(TRUE, 4), rep(FALSE, 2))

#### Results matrix ####

#### Shizuka boot ####
Results <- data.frame(array(data = 0, dim = c(nrow(loaded_networks), 4), dimnames = list(NULL, c("Data", "Shizuka_boot", "Ascanio_boot", "Ascanio_perm"))))

for(i in 1:nrow(loaded_networks)) {
    Results$Data[i] <- gsub("_g", "", loaded_networks$Graph[i])
    Results$Shizuka_boot[i] <- calc_rc(data = eval(parse(text = loaded_networks$RawData[i])), 
                                       n.bootstraps = 100, gbi.format = loaded_networks$gbi.format[i])
    
    
    #### Ascanio boot ####
    
    Pij_b <- calc_Pmatrix(data = eval(parse(text = loaded_networks$Graph[i])),
                          n.permutations =  100, algorithm = 'fastgreedy', 
                          permutations = F, boot.data = eval(parse(text = loaded_networks$RawData[i])),
                          gbi.format = loaded_networks$gbi.format[i])
    
    Exx_b <- rcomm_exx(Pij_b)
    Exy_b <- rcomm_exy(Pij_b)
    
    Results$Ascanio_boot[i] <- (Exx_b - Exy_b)/(1 - Exy_b)
    
    #### Ascanio permutation ####
    
    
    Pij_p <- calc_Pmatrix(data = eval(parse(text = loaded_networks$Graph[i])),
                          n.permutations =  100, algorithm = 'fastgreedy', 
                          permutations = T, boot.data = eval(parse(text = loaded_networks$RawData[i])),
                          gbi.format = loaded_networks$gbi.format[i])
    
    Exx_p <- rcomm_exx(Pij_p)
    Exy_p <- rcomm_exy(Pij_p)
    
    Results$Ascanio_perm[i] <- (Exx_p - Exy_p)/(1 - Exy_p)
    
}
