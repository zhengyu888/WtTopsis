#' GetWeight
#' Calculate weights on standardized data.
#' @param data A Normalized data set
#'
#' @return WeightEij is Ej value of Index,weight is weight value of Index
#' @export GetWeight
#'
#' @examples
#' \dontrun{
#' i=10
#' GetWeight(sampleData)
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
