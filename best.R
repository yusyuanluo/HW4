best<-function(state,outcome){
  ##read outcome data
  path<-paste(getwd(),"/", "outcome-of-care-measures.csv", sep="")
  data<-read.csv(path, colClasses = "character")
  newdata<-as.data.frame(cbind(data[,2],   ##hospital name
                               data[,7],   ##states
                               data[,11],  ##heart attack
                               data[,17],  ## heart failure 
                               data[,23]))  ## pneumonia
  colnames(newdata)<-c("hospital", "states","heart attack","heart failure", "pneumonia")
  ##check that state and outcome are valid
  if (!state %in% newdata[,"states"]){
    stop("invalid state")
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  } else{
    splitstatelocation<-which(newdata[,"states"]==state)
    splitdata<-newdata[splitstatelocation,]
    endpoint<-as.numeric(splitdata[,eval(outcome)])
    min_val<-min(endpoint,na.rm=TRUE)
    result<-splitdata[,"hospital"][which(endpoint==min_val)]
    output<-result[order(result)]}
    return(output)
  }
  