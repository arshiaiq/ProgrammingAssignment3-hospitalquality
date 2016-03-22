
lrankall <- function(outcome,num = "best") {
  
  hospfile <- read.csv ("outcome-of-care-measures.csv",stringsAsFactors = FALSE, na.strings = "Not Available")
  col_index <- c("heart attack" = 11,"heart failure" = 17,"pneumonia" = 23) #column indexes as per master file
  hranking <- data.frame() #hospital dataframe with ranks for all hospitals by outcome
  
      if ((outcome %in% names(col_index))==FALSE) #check for validity of outcome
        stop("invalid outcome", call. = TRUE)

  hosp_state <- subset(hospfile, select = c(2,7,col_index[[outcome]])) 
  hosp_order <- hosp_state[order(hosp_state[,3],hosp_state[,1], na.last = NA),]
  
  hstate <- split(hosp_order,hosp_order[,2])      
  
  hospname <- function(x) {
    
    if(num == "best") {num = 1} #num values for best and worst
      else if(num == "worst") {num = length(x[,2])}
    
    x[,1][num] #select hospital name based on num value
  }
  
  hosp_name <- sapply(hstate,hospname)
  data.frame(hospital = hosp_name, state = names(hosp_name))
}   
