pollutantmean <- function(directory, pollutant, id = 1:332) {
    #check if id is valid
    if( id[1] < 0 | id[length(id)] > 332 ) {
        stop("Wrong index!!!")
    }
    
    means <- vector("numeric",length=0)
    
    for( i in id) {
        if( i < 10 )
            filename <- paste("00",i,sep="")
        else if ( i < 100 )
                filename <- paste("0",i,sep="")
            else
                filename <- i
        filename <- paste(directory,"/",filename,".csv",sep="")
        
        d <- read.csv(filename)
        means <- c(means,d[,pollutant])
    }
    
    mean(means,na.rm=TRUE)
}