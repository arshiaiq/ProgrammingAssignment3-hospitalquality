
best <- function(state,outcome) {
  
    hospfile <- read.csv ("outcome-of-care-measures.csv",stringsAsFactors = FALSE, na.strings = "Not Available")
    hstate <- unique(hospfile$State)
    houtcome <- c("heart attack","heart failure","pneumonia")
    col_index <- c("heart attack" = 11,"heart failure" = 17,"pneumonia" = 23) #column indexes as per master file
    
    if (any(state==hstate)==FALSE) #check for validity of state
      stop("invalid state", call. = TRUE)
    
    if (any(outcome==houtcome)==FALSE) #check for validity of outcome
      stop("invalid outcome", call. = TRUE)
    
    hosp_state <- subset(hospfile, State == state, select = c(2,col_index[[outcome]])) #columns selected based on outcome
    hosp_order <-  hosp_state[order(hosp_state[,2],hosp_state[,1],na.last = NA),]  #sort columns by outcome values and by hospital name
    hosp_name <- hosp_order[,1][which.min(hosp_order[,2])] #select the hospital name by minimum value 
    hosp_name
  } 