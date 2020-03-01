best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ### extracting particular outcomes
  outcome1   <- as.data.frame(cbind(data[, 2],   # hospital
                              data[, 7],   # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  ## Allotingcolumn names to the extracted data
  colnames(outcome1) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% outcome1[, "state"]){
    stop('invalid state')
  } 
  else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } 
  else {
    a <- which(outcome1[, "state"] == state)
    b <- outcome1[a, ]    ## data selected for a given state
    c <- as.numeric(b[, eval(outcome)])
    min_val <- min(c, na.rm = TRUE)
    result  <- b[, "hospital"][which(c == min_val)]
    output  <- result[order(result)]
  }
  return(output)
}