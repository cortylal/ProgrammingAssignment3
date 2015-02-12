## This function's goal is to return the name of the Hospital that has the 
## ranking "num", considering the morality rate for a specified condition 
## in a specific US state

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data, convert "Not Available" into proper NAs for removal later
        complete_outcome <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        
        ## Check that state and outcome are valid
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
        
        
        
        ## Setting the requested rank as numeric
        if (num == "best") {
                num <- 1
        }
        if (num == "worst") {
                num <- nrow(wanted_data)
                
                ##last <- as.vector(tail(n=1,complete_outcome[,"Hospital.Name"]))
        }
        if (num > nrow(wanted_data)) {
                return(NA)
        }
        
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
}

## This function is used to toupper every first letter of a sentence
## Taken from ?toupper
.simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
}