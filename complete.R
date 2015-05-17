complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  data <- matrix(nrow = 0, ncol = 2)
  
  for(i in id) {
    
    filename = paste(directory,"/",formatC(i, width = 3, format = "d", flag = "0"),".csv",sep = "", collapse = NULL)
    filedata <- file(description = filename, open = "r")
    datatemp <- read.csv(filedata)
    close(filedata)
    
    correct1<-(as.numeric(!is.na(datatemp[,2])))
    correct2<-(as.numeric(!is.na(datatemp[,3])))
    correct<- sum(correct1*correct2)
    
    data <- rbind(data,c(i,correct))
  }
  colnames(data) <-  c("id", "nobs") 
  ##rownames(data) <-  paste("##",c(id))
  data<-as.data.frame(data)
  
  return(data)
}
