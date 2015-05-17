pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  data <- matrix(nrow = 0, ncol = 4) 
  
  for(i in id) {
    
    filename = paste(directory,"/",formatC(i, width = 3, format = "d", flag = "0"),".csv",sep = "", collapse = NULL)
    filedata <- file(description = filename, open = "r")
    datatemp <- read.csv(filedata)
    close(filedata)
    
    data <- rbind(data,datatemp)
  }
  
  if (pollutant=="sulfate") pol<-2 else if (pollutant=="nitrate") pol<-3 else pol<-NA
  output<-mean(data[,pol],na.rm=TRUE)
  
  return(output)
}