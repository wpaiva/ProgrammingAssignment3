rankall <- function(outcome, num = "best") {
        options(warn = -1) # disable warnings

        ## internal variables
        valid_outcome <- list(c("heart attack", "heart failure", "pneumonia"),c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
        outcomes <- c('heart attack', 'heart failure', 'pneumonia')
        indices <- c(11, 17, 23)
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")

        ## Check that state and outcome are valid
        if (nrow(data[data$State == state,]) == 0) {
                stop("invalid state")
        } else if (length(valid_outcome[[1]][valid_outcome[[1]] == outcome]) == 0) {
                stop("invalid outcome")
        } 
        
        i <- indices[match(outcome, outcomes)]
        hospitals <- data[, c(2, 7, i)]
        hospitals[, 3] <- as.numeric(as.character(hospitals[, 3]))
        hospitals <- na.omit(hospitals)
        names(hospitals) <- c("hospital", "state", "rate")
        
        if (num == "best") {
                num <- 1
        } else if (num == "worst") {
        } else {
                num <- as.numeric(num)
                if (is.na(num)) {
                        stop("invalid num")
                } else if (num > nrow(hospitals)) {
                        return(NA)
                }
        }
        
        results <- NULL

        ## For each state, find the hospital of the given rank
        for(state in levels(hospitals$state)) {
                hospitals_for_state <- hospitals[hospitals$state == state, ]
                
                if (num == "worst") {
                        n <- nrow(hospitals_for_state)
                } else {
                        n <- num
                }
                result <- hospitals_for_state[order(hospitals_for_state$rate, hospitals_for_state$hospital), c(1, 2)][n, ]
                result$state <- rep(state, nrow(result))
                results <- rbind(results, result)  
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        rownames(results) <- NULL
        return(results)
}