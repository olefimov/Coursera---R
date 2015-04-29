corr <- function(directory, threshold = 0) {

    files <- list.files(directory);
    tf <- length(files)
    corrs <- vector(length=0)
    
    for( i in 1:tf) {
        df <- na.omit(read.csv(paste(directory,"/",files[i],sep="")))
        if(nrow(df) > threshold ) {
            corrs <- c(corrs,cor(df$sulfate,df$nitrate))
        }
    }
    
    return(corrs)
}