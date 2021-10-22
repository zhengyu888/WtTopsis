#' DataNormalpre
#' Prepare the normalized data set.
#' @param data A maximized data set
#' @param i Index column
#'
#' @return Index column normalized
DataNormalpre<-function(data,i){
  sampleData2<-data
  i=i
  sampleData3<-sampleData2[,c(1,i)]
  sampleData3$pingfang<-(sampleData3[,2])**2
  sumpingfang<-sum(sampleData3$pingfang)
  sqrtsumpingfang<-sqrt(sumpingfang)
  sampleData3$BZZH<- (sampleData3[,2])/sqrtsumpingfang
  sampleData4<-sampleData3[,c(1,4)]
  DataBZH<-sampleData4
  names(DataBZH)[1] <- names(sampleData2[1])
  names(DataBZH)[2] <- names(sampleData2[i])
  return(DataBZH)
}
