# --- 
# title: "cmbDFrame" 
# author: "Emanuel Valero, Alfredo Ascanio" 
# date: "08/04/2015"
# e-mail: "11-10060@usb.ve"
# --- 

## cmbDFrame function takes an 'x' data.frame and makes a list 'op'
## that contains two data.frames with two columns each.
## These dataframes consist of two column each, where each row is 
## a link between nodes, given that those nodes appeared in the same row
## in matrix 'x'. The dataframe 'mrgCombs' contains links with NA values
## while the dataframe 'mrgCCombs' contains only complete rows (without NA's)

cmbDFrame <- function(x) { 
        c <- x 
        
        oput <- list()
        for(i in 1:nrow(c)) {
                out <- combn(as.matrix(c[i,]), 2)
                oput[[i]] <- t(out)
        }
        
        for(i in 1:length(oput)) {
                
                if(sum(!is.na(oput[[i]][, 2])) == 0) {
                        oput[[i]] <- head(oput[[i]], 1)
                        oput[[i]][is.na(oput[[i]])] <- ""
                }
        }
        
        mrg <- do.call("rbind", oput)
        mrgC <- mrg[complete.cases(mrg),]
        
        op <- list(
                mrgCombs = mrg, mrgCCombs = mrgC
        )
        
        return(op)
}