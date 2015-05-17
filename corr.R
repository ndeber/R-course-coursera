corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!

  data <- vector("numeric", length = 0) 
  list <- complete("specdata")
  
  for(i in 1:332) {
    if(list[i,2]>=threshold) { 
      filename = paste(directory,"/",formatC(i, width = 3, format = "d", flag = "0"),".csv",sep = "", collapse = NULL)
      filedata <- file(description = filename, open = "r")
      datatemp <- read.csv(filedata)
      close(filedata) 
      
      cortemp<- cor(datatemp[,2],datatemp[,3],use = "pairwise.complete.obs")
      data <- c(data,cortemp)
    }
  }
  
 return(data)
}
