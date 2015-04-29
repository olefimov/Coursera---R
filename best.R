best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",header=TRUE,colClass="character")
    
    ## Check that state and outcome are valid
    outcome_list <- c("heart attack", "heart failure", "pneumonia")
    
    if( (state %in% data$State) == FALSE ) {
        stop("invalid state")
    }
    
    if( (outcome %in% outcome_list) == FALSE ) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    state_hospitals = subset(data,data$State == state)
    
    if( outcome == "heart attack" ) {        
        state_hospitals <- subset(state_hospitals,state_hospitals[,11] != "Not Available")
        as.numeric(state_hospitals[,11])
        min_d <- min(state_hospitals[,11],na.rm=TRUE)
        x<-subset(state_hospitals,state_hospitals[,11] == min_d)
    }
    else 
        if( outcome == "heart failure" ) {
            state_hospitals <- subset(state_hospitals,state_hospitals[,17] != "Not Available")
            as.numeric(state_hospitals[,17])
            min_d <- min(as.numeric(state_hospitals[,17]),na.rm=TRUE)
            x<-subset(state_hospitals,state_hospitals[,17] == min_d)
        }
        else {
            state_hospitals <- subset(state_hospitals,state_hospitals[,23] != "Not Available")
            as.numeric(state_hospitals[,23])
            min_d <- min(as.numeric(state_hospitals[,23]),na.rm=TRUE)
            x<-subset(state_hospitals,state_hospitals[,23] == min_d)
        }
    
    x <- x[order(x$Hospital.Name),]  # sort list by Hospital.Name
    x <- x[1,]                       # get the first row of the sorted list
    x$Hospital.Name
}