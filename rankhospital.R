
rankhospital <- function(state,outcome,num = "best") {
  
  hospfile <- read.csv ("outcome-of-care-measures.csv",stringsAsFactors = FALSE, na.strings = "Not Available")
  hstate <- unique(hospfile$State)
  col_index <- c("heart attack" = 11,"heart failure" = 17,"pneumonia" = 23) #column indexes as per master file
  
      if ((state %in% hstate)==FALSE) #check for validity of state
        stop("invalid state", call. = TRUE)
      
      if ((outcome %in% names(col_index))==FALSE) #check for validity of outcome
        stop("invalid outcome", call. = TRUE)
  
  hosp_state <- subset(hospfile, State == state, select = c(2,col_index[[outcome]])) #columns selected based on outcome
  hosp_order <- hosp_state[order(hosp_state[,2],hosp_state[,1],na.last = NA),] #sort columns by outcome values and by hospital name
  
      if(num == "best") {num = 1} #num values for best and worst
       else if(num == "worst") {num = length(hosp_order[,2])}
  
  hosp_name <- hosp_order[,1][num] #select the hospital name by num value specified
  hosp_name
}  
