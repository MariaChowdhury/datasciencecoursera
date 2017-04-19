complete<-function(directory,id=1:332){
  filenames<-list.files(directory,full.names=TRUE)
  nobs<-numeric()
  for(i in id ){
    nobs<-c(nobs,sum(complete.cases(read.csv(filenames[i]))))
  }
  print(data.frame(id,nobs))
  
}
