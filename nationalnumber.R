## This function's goal is to return the number of patient for a given oucome
## in the entire USA. Uses patientnumber() from numberpatient.R

nationalnumber <- function(outcome) {
        ## Check that the outcome is valid
        ##
        ## Is the outcome valid?
        if(outcome != "heart attack" & outcome != "heart failure" 
           & outcome != "pneumonia") {
                stop("invalid outcome")
        }
        
        ## Read outcome data, but only keep the State column
        complete_outcome <- read.csv("outcome-of-care-measures.csv", 
                                     colClasses = c(rep("NULL",6), "character", rep("NULL", 39)), 
                                     header = TRUE)
        ## Create the vector of different states
        state <- character(0)
        for (s_name in unique(complete_outcome$State)) {
                state <- c(state, s_name)
        }
        
        ## Create the total number to return
        national_number <- 0
        
        ## For each state, get the number of patient for the outcome and add to national_number
        for (s_name in state) {
                national_number <- national_number + patientnumber(s_name, outcome)
        }
        
        ## Return the total number of patient treated for the outcome in the USA
        return(national_number)
}