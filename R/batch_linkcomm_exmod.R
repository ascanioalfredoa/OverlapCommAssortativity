batch.linkcomm.exmod <- function(x){ #e.g. x <- '_g'
    
    all_networks <- mget(ls(pattern = x, pos= 1L),  envir = as.environment(1L))
    
    all_networks %>% #number of graphs objects
        map(linkcomm.exmod) #%>% 
        #map(getLinkCommunities, plot = FALSE) %>% 
        #map(extract2, 'numbers') %>% 
        #map(extract, 3) %>%  #position of the number of communities
        #unlist() %>% 
        #data.frame() %>% 
        #rownames_to_column() %>% 
        #rename(network = rowname, over_comm = '.')
    
}