pollutantmean<-function(directory,pollutant,id=1:332){
  
  #collecting all the file names in specdata directory
  filenames<-list.files(directory,full.names=TRUE)
  
  #variable to hold the output data 
  mydata<-numeric()
  
  for(i in id ){
    data<-read.csv(filenames[i])
    mydata<-c(mydata,data[[pollutant]])
  }
  print(mean(mydata,na.rm=TRUE))
  
}

