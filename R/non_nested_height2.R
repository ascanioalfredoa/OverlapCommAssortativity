# --- 
# title: "getMaxden" 
# author: Alfredo Ascanio
# date: "02/10/2017"
# e-mail: "11-10060@usb.ve"
# --- 

## 'getMaxden' function takes a linkcomm class object produced with the function
## 'getLinkCommunities' in the linkcomm package. 'getMaxden' makes an iterative
## process to get the maximum density for non-nested linked communities.

getMaxden <- function(x){
  
  require(linkcomm) 
  a <- data.frame(x$pdens)
  a <- a[order(a$pdens, decreasing = T), 1]
  b <- NULL 
  j = 1 
  for(i in 1:length(a)) { 
    
    if( a[i] == 0) {next()} # if the value is 0,then skip to next iteration
    
    test <- try({y <- getAllNestedComm(newLinkCommsAt(x, cutat = a[i]))}, silent = TRUE) #Nested communities for i treshold
    if(class(test) != "list") {next()}
    
    if(length(test) > 0) {next()} else { # If there still exist nested communities, we go through another iteration
      
      b[j] <- a[i] 
      j = j + 1 
    }
    if(j > 1) {break()}
  } 
  
  
  z <- x$pdens[(x$pdens[, 1] %in% b),]
  z
}
