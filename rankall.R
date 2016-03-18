rankall <- function(outcome,num = "best") {
  
  hospfile <- read.csv ("outcome-of-care-measures.csv",stringsAsFactors = FALSE, na.strings = "Not Available")
  hstate <- sort(unique(hospfile$State))
  houtcome <- c("heart attack","heart failure","pneumonia")
  col_index <- c("heart attack" = 11,"heart failure" = 17,"pneumonia" = 23) #column indexes as per master file
  hranking <- data.frame() #hospital dataframe with ranks for all hospitals by outcome
  
  if (any(outcome==houtcome)==FALSE) #check for validity of outcome
    stop("invalid outcome", call. = TRUE)
  
  for (i in hstate) 
    {
      hosp_state <- subset(hospfile, State == i, select = c(2,col_index[[outcome]])) #columns selected based on outcome
      hosp_order <- hosp_state[order(hosp_state[,2],hosp_state[,1], na.last = NA),]  #sort columns by outcome values and by hospital name
      
      number = num
      
      if(number == "best")  {number = 1} #num values for best and worst
      if(number == "worst") {number = length(hosp_order[,2])}           
      
      hosp_name <- hosp_order[,1][match(hosp_order[,2][number],hosp_order[,2],nomatch = NA)] #select the hospital name by num value specified
      hranking <- rbind(hranking, as.data.frame(hosp_name, stringsasFactors = FALSE))
    } 
  
  hranking <- cbind(hranking,hstate)
  colnames(hranking) <- c("hospital","state")
  hranking
}