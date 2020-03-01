rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome1   <- as.data.frame(cbind(data[, 2],  # hospital
                              data[, 7],  # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(outcome1) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state %in% outcome1[, "state"]) {
    stop('invalid state')
  } 
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } 
  else if (is.numeric(rank)) {
    a <- which(outcome1[, "state"] == state)
    b <- outcome1[a, ]
    b[, eval(outcome)] <- as.numeric(b[, eval(outcome)])
    b <- b[order(b[, eval(outcome)], b[, "hospital"]), ]
    ranking <- b[, "hospital"][rank]
  } 
  else if (!is.numeric(rank)){
    if (rank == "best") {
      ranking <- best(state, outcome)
    } 
    else if (rank == "worst") {
      a <- which(outcome1[, "state"] == state)
      b <- outcome1[a, ]    
      b[, eval(outcome)] <- as.numeric(b[, eval(outcome)])
      b <- b[order(b[, eval(outcome)], b[, "hospital"], decreasing = TRUE), ]
      ranking <- b[, "hospital"][1]
    } 
    else {
      stop('invalid rank')
    }
  }
  return(ranking)
}
