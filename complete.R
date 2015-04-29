complete <- function(directory, id = 1:332) {

    result <- data.frame(id=numeric(),nobs=numeric()) 
    
    for( i in id) {
        if( i < 10 )
            filename <- paste("00",i,sep="")
        else if ( i < 100 )
            filename <- paste("0",i,sep="")
        else
            filename <- i
        filename <- paste(directory,"/",filename,".csv",sep="")
        
        d <- na.omit(read.csv(filename));   
        result <- rbind(result,data.frame(id=i,nobs=nrow(d)))
    }
    result
}