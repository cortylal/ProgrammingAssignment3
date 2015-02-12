## This function's goal is to return the name of the Hospital that has the 
## ranking "num", considering the mortality rate for a specified condition 
## in a specific US state

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data, convert "Not Available" into proper NAs for removal later
        complete_outcome <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        
        ## Check that state and outcome are valid
        ##
        if (!(state %in% complete_outcome$State)) {                     ## Is the state in the data frame?    
                stop("invalid state")
        }
        if(outcome != "heart attack" & outcome != "heart failure"       ## Is the outcome valid?
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
        ## Sorting the whole by death rate first, by hospital name if there's a tie 
        ordered_data <- wanted_data[order(wanted_data[,outcome_column_name],wanted_data[,"Hospital.Name"]),]
        
        ## Setting the rank as numeric
        ##
        if (num == "best") {    ## Return the first name
                num <- 1
        }
        if (num == "worst") {   ## Return the last name 
                num <- nrow(ordered_data)
        }
        
        ## Return hospital name in the given rank
        ##
        hospital_name <- as.vector(ordered_data[num,"Hospital.Name"])
        return(hospital_name)
}

## This function is used to toupper every first letter of a sentence
## Taken from ?toupper
.simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
}