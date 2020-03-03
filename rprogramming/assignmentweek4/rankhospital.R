rankhospital <- function(state, outcome, num = "best") { 
    outcomedata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    ## Read outcome data
    if(!(state %in% outcomedata[, 7])) {
        stop("invalid state")
    }
    if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    ## Check that state and outcome are valid
    cleanoutcome <- outcomedata[, c(2, 7, 11, 17, 23)]
    names(cleanoutcome) <- c("hospital", "statename", "attack", "failure", "pneumonia")
    state.outcome <- split(cleanoutcome, cleanoutcome$statename)
    stateout <- state.outcome[[state]]
    if(num %in% "worst") {
        descending <- TRUE
    } else {
        descending <- FALSE
    }
    if(outcome == "heart attack") {
        rankeddata <- stateout[order(stateout$attack, stateout$hospital, decreasing = descending),]
    } else if(outcome == "heart failure") {
        rankeddata <- stateout[order(stateout$failure, stateout$hospital, decreasing = descending),]
    } else if(outcome == "pneumonia") {
        rankeddata <- stateout[order(stateout$pneumonia, stateout$hospital, decreasing = descending),]
    }
    if(num %in% c("best", "worst")) {
        print(rankeddata[1, 1])
    } else {
        print(rankeddata[num, 1])
    }
    ## Return hospital name in that state with the given rank 
    ## 30-day death rate
}