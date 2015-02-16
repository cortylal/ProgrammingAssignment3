## This function's goal is to return the number of patients that
## are received in a state for a specific condition (outcome)

patientnumber <- function(state, outcome) {
        ## Read outcome data, convert "Not Available" into proper NAs for removal later
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
        
        ## Generate corresponding outcome column name from the function's call parameter 
        ##
        outcome_column_name <- .simpleCap(outcome)         ## toupper every first letter of a sentence
        outcome_column_name <- gsub(" ",".", outcome_column_name)      ## Replacing spaces with dotes 
        outcome_column_name <- paste("Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from", outcome_column_name, sep = ".")
        
        ## First, select the hospitals of the given state
        state_outcome <- subset(complete_outcome, subset = complete_outcome$State == state)
        ## Only keep the wanted outcome results
        wanted_data <- state_outcome[,c("Hospital.Name", outcome_column_name)]
        ## Remove the NAs in outcome_column_name
        wanted_data <- wanted_data[!is.na(as.numeric(as.character(wanted_data[,outcome_column_name]))),]
        
        
        ## Sum the number of patients for the specified outcome
        npatients <- sum(wanted_data[,outcome_column_name])
        
        ## Return the number of patient 
        return(npatients)
}

## This function is used to toupper every first letter of a sentence
## Taken from ?toupper
.simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
}