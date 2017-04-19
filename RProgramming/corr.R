corr <- function(directory, threshold = 0) {
  
  # empty corrsNum numeric vector
  corrsNum <- numeric(0)
  
  # collecting complete data frame 
  nobsComplete <- complete("specdata")
  
  # data frame based on threshold
  nobsComplete <- nobsComplete[nobsComplete$nobs > threshold, ]
  
  
  for (i in nobsComplete$id) {
    
    filename<-paste(directory, "/", sprintf("%03d", as.numeric(i)), ".csv", 
                    sep = "")
    
    #data frame for each file
    data<-read.csv(filename)
    
    corrsNum <- c(corrsNum, cor(data$sulfate, data$nitrate, use = "pairwise.complete.obs"))
  }
  
  
  print(corrsNum)
}

complete<-function(directory,id=1:332){
  
  #files in directory
  filenames<-list.files(directory,full.names=TRUE)
  
  #empty nobs to hold nobs
  nobs<-numeric()
  
  for(i in id ){
    nobs<-c(nobs,sum(complete.cases(read.csv(filenames[i]))))
  }
  print(data.frame(id,nobs))
  
}

