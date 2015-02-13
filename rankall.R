## This function's goal is to return the name of the Hospitals for each state,
## given a specified outcome and ranking

rankall <- function(outcome, num = "best") {
        ## Check that the outcome is valid
        ##
        if(outcome != "heart attack" & outcome != "heart failure"       ## Is the outcome valid?
           & outcome != "pneumonia") {
                stop("invalid outcome")
        }
        
        ## Read outcome data, convert "Not Available" into proper NAs for removal later
        complete_outcome <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        ## Create the data.frame to return (now empty)
        name_by_state <- data.frame(Hospital.Name = character(0), State = character(0), stringsAsFactors = FALSE)
                
        ## Generate corresponding outcome column name from the function's call parameter 
        ##
        outcome_column_name <- .simpleCap(outcome)         ## toupper every first letter of a sentence
        outcome_column_name <- gsub(" ",".", outcome_column_name)      ## Replacing spaces with dotes 
        outcome_column_name <- paste("Hospital.30.Day.Death..Mortality..Rates.from", outcome_column_name, sep = ".")
        
        ## Only keep the wanted variables from the complete initial data
        wanted_data <- complete_outcome[,c("State", "Hospital.Name", outcome_column_name)]
        
        ## For each state, find the hospital of the given rank
        ##
        for (s_name in unique(wanted_data$State)) {
                ## First, select the hospitals of the given state
                state_outcome <- subset(wanted_data, subset = wanted_data$State == s_name)
                ## Remove the NAs in outcome_column_name
                state_outcome <- state_outcome[!is.na(as.numeric(as.character(state_outcome[,outcome_column_name]))),]
                ## Sorting the whole by death rate first, by hospital name if there's a tie 
                ordered_data <- state_outcome[order(state_outcome[,outcome_column_name],state_outcome[,"Hospital.Name"]),]
                ## Setting the rank as numeric
                ##
                if (num == "best") {    ## Return the first name
                        temp_num <- 1
                }
                else if (num == "worst") {   ## Return the last name 
                        temp_num <- nrow(ordered_data)
                }
                else if (num > nrow(ordered_data)) {         ## The data is unavailable 
                        hospital_state <- c(NA,s_name)
                        name_by_state <- rbind(name_by_state, hospital_state)
                        next
                }
                else {
                        temp_num <- num
                }
                
                hospital_state <- as.vector(ordered_data[temp_num,c("Hospital.Name","State")])
                ## Adding the corresponding (Hospital.Name, State) pair to the final data frame
                name_by_state <- rbind(name_by_state, hospital_state)
        }
        
        ## Rename the columns as required
        names(name_by_state) <- c("hospital", "state")
        ## Order the data frame by state name
        name_by_state <- name_by_state[order(as.character(name_by_state[,"state"])),]
        ## Return a data frame with the hospital names and the (abbreviated) state name
        return (name_by_state)
}

## This function is used to toupper every first letter of a sentence
## Taken from ?toupper
.simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
}