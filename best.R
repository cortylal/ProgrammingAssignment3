## This function's goal is to return the name of the Hospital which has the
## lowest mortality for a specified condition, and in a specific US state

best <- function(state, outcome) {
        ## Read outcome data
        complete_outcome <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        ##
        ## Is the state in the data frame?
        if (!(state %in% complete_outcome$State)) {     
                stop("invalid state")
        }
        ## Is the outcome valid?
        if(outcome != "heart attack" & outcome != "heart failure" 
           & outcome != "pneumonia") {
                stop("invalid outcome")
        }
        
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}