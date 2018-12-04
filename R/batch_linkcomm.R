batch_linkcomm <- function(x){ #x <- '_g'
    
    all_networks <- list(mget(ls(pattern = x)))
    
    all_networks[[1]][1:2] %>%
        map(get.edgelist) %>% 
        map(getLinkCommunities, plot = FALSE) %>% 
        map(extract2, 'numbers') %>% 
        map(extract, 3) %>%  #position of the number of communities
        unlist() %>% 
        data.frame() %>% 
        rownames_to_column() %>% 
        rename(network = rowname, over_comm = '.')
        
}