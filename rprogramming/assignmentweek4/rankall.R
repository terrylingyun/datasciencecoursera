rankall <- function(outcome, num = "best") { 
    outcomedata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    ## Read outcome data
    if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    ## Check that state and outcome are valid
    cleanoutcome <- outcomedata[, c(2, 7, 11, 17, 23)]
    names(cleanoutcome) <- c("hospital", "state", "attack", "failure", "pneumonia")
    if(num %in% "worst") {
        descending <- TRUE
        num <- 1
    } else if (num %in% "best") {
        descending <- FALSE
        num <- 1
    } else {
        descending <- FALSE
    }
    if(outcome == "heart attack") {
        rankeddata <- cleanoutcome[order(cleanoutcome$attack, cleanoutcome$hospital, decreasing = descending),]
    } else if(outcome == "heart failure") {
        rankeddata <- cleanoutcome[order(cleanoutcome$failure, cleanoutcome$hospital, decreasing = descending),]
    } else if(outcome == "pneumonia") {
        rankeddata <- cleanoutcome[order(cleanoutcome$pneumonia, cleanoutcome$hospital, decreasing = descending),]
    }
    state.outcome <- split(rankeddata, rankeddata$state)
    listhospital <- list()
    liststate <- list()
    for(statename in state.outcome) {
        listhospital <- c(listhospital, statename[num, 1])
        liststate <- c(liststate, statename[num, 2])
    }
    output <- cbind(listhospital, liststate)
    output <- as.data.frame(output)
    names(output) <- c("hospital", "state")
    ## For each state, find the hospital of the given rank
    output
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}