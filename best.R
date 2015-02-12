## This function's goal is to return the name of the Hospital which has the
## lowest mortality for a specified condition, and in a specific US state

best <- function(state, outcome) {
        ## Read outcome data, convert "Not Available" into proper NAs for removal later
        ##
        complete_outcome <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        
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
        
        ## Sort the hospitals of given state by best rates for the specific condition
        ##
        ## First, select the hospitals of the given state
        state_outcome <- subset(complete_outcome, subset = complete_outcome$State == state)
        ##
        ## Generate corresponding outcome column name from the function's call parameter 
        outcome_column_name <- .simpleCap(outcome)         ## toupper every first letter of a sentence
        outcome_column_name <- gsub(" ",".", outcome_column_name)      ## Replacing spaces with dotes 
        outcome_column_name <- paste("Hospital.30.Day.Death..Mortality..Rates.from", outcome_column_name, sep = ".")
        ## Only keep the wanted outcome results
        wanted_data <- state_outcome[,c("Hospital.Name", outcome_column_name)]
        ## Remove the NAs in outcome_column_name
        wanted_data <- wanted_data[!is.na(as.numeric(as.character(wanted_data[,outcome_column_name]))),]
                
        ## Then, find the min for the specified condition (NAs removed previously)
        min_death_rate <- min(as.numeric(as.character(wanted_data[,outcome_column_name])))

        ## Create a vector of the names of the best hospitals in state 
        ##  
        top_outcome <- as.vector(wanted_data[as.numeric(as.character(wanted_data[,outcome_column_name])) == min_death_rate, "Hospital.Name"])
        
        ## If there are ties, sort hospitals by name and return the first
        ##
        if (length(top_outcome) > 1) {
                sorted_names <- sort(top_outcome)
                return(sorted_names[1])
        }
        
        ## Else return the unique solution
        ##
        return(top_outcome)
}

## This function is used to toupper every first letter of a sentence
## Taken from ?toupper
.simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
}