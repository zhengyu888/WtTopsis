#' GetWeight
#' Calculate weights on standardized data us the entropy method.
#' @param data A Normalized data set
#'
#' @return WeightEij is Ej value of Index,weight is weight value of Index
#' @export GetWeight
#'
#' @examples
#' \donttest{
#' sampleData<-sampleData
#' mth<-c("ZH","ZJ","QJ","ZH","FU","ZH","QJ","FU","FU","ZH")
#' zmth<-c(NA,NA,NA,NA,"CZ",NA,NA,"DS","CZ",NA)
#' xbest<-c(NA,4,NA,NA,NA,NA,NA,NA,NA,NA)
#' qup<-c(NA,NA,5,NA,NA,NA,5,NA,NA,NA)
#' qlow<-c(NA,NA,3,NA,NA,NA,3,NA,NA,NA)
#' weightdata<-data2max(sampleData,mth,zmth,xbest,qlow,qup)
#' GetWeight(weightdata)
#' }
GetWeight<-function(data){
  weightd<-data
  nc<-ncol(weightd)
  weighti<-NULL
  for (i in 2:nc){
    Eji<-Ejcaul(weightd,i=i)
    names(Eji)<-names(weightd)[i]
    weighti=c(weighti,Eji)
  }
  WeightEij<-weighti
  WeightEij<-data.frame(WeightEij)
  WeightEij<-t(WeightEij)
  weight<-(1-WeightEij[1,])/((nc-1)-sum(WeightEij[1,]))
  weight<-data.frame(weight)
  weight<-t(weight)
  Wi<-rbind(WeightEij,weight)
  return(Wi)
}
