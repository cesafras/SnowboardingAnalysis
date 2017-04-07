best <- function(state,outcome) {
  data <- read.csv("outcome-of-care-measures.csv",na.strings = "Not Available")
  
  state <- toupper(state)
  
  validstate <- data[data$State==state,"State"]
  if (length(validstate)==0) {
   stop("invalid state")
  }
 
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }
  
  outcome <- simpleCap(outcome)
  outcomeinput <- gsub("[ ]",".",outcome)
  outcomecol <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcomeinput,sep="")
  
  if (!(outcomecol %in% colnames(data))) {
    stop("invalid outcome")
  }
  
  subsetdata <- data[data$State==state,c("Hospital.Name",outcomecol)]
  cleansubsetdata <- subsetdata[complete.cases(subsetdata),]
  orderdata <- cleansubsetdata[order(cleansubsetdata[,2],cleansubsetdata[,1]),]
  rankdata <- cbind(orderdata,"rank"=1:nrow(orderdata))
  return(as.character(rankdata[rankdata$rank==1,"Hospital.Name"]))
}