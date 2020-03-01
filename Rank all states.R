rankall <- function(outcome, num = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome1   <- as.data.frame(cbind(data[, 2],  # hospital
                              data[, 7],  # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(outcome1) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  outcome1[, eval(outcome)] <- as.numeric(outcome1[, eval(outcome)])
  
  ## Check that state and outcome are valid
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } 
  else if (is.numeric(num)) {
    divstate <- with(outcome1, split(outcome1, state)) ## split the data by state wise
    ordered  <- list()
    for (i in seq_along(divstate)){
      divstate[[i]] <- divstate[[i]][order(divstate[[i]][, eval(outcome)], 
                                           divstate[[i]][, "hospital"]), ]
      ordered[[i]]  <- c(divstate[[i]][num, "hospital"], divstate[[i]][, "state"][1])
    }
    result <- do.call(rbind, ordered)
    rankall <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
    names(rankall) <- c("hospital", "state")
  } 
  else if (!is.numeric(num)) {
    if (num == "best") {
      divstate <- with(outcome1, split(outcome1, state))
      ordered  <- list()
      for (i in seq_along(divstate)){
        divstate[[i]] <- divstate[[i]][order(divstate[[i]][, eval(outcome)], 
                                             divstate[[i]][, "hospital"]), ]
        ordered[[i]]  <- c(divstate[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      rankall <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(rankall) <- rankall[, 2]
    } 
    else if (num == "worst") {
      divstate <- with(outcome1, split(outcome1, state))
      ordered  <- list()
      for (i in seq_along(divstate)){
        divstate[[i]] <- divstate[[i]][order(divstate[[i]][, eval(outcome)], 
                                             divstate[[i]][, "hospital"], 
                                             decreasing = TRUE), ]
        ordered[[i]]  <- c(divstate[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      rankall <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(rankall) <- rankall[, 2]
    }
    else {
      stop('invalid num')
    }
  }
  return(rankall)
}


