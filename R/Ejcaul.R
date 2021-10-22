#' Ejcaul
#' Calculate Ej value.
#' @param data A normalized data set.
#' @param i Index column.
#'
#' @return Index column weight Ej
Ejcaul<-function(data,i){
  WeightData<-data
  i=i
  nr<-nrow(WeightData)
  WeightData2<-WeightData[,c(1,i)]
  SumX<-sum( WeightData2[,2])
  WeightData2$Pij<-WeightData2[,2]/SumX
  WeightData2$PijLN<-(WeightData2$Pij)*log(WeightData2$Pij)
  WeightData2[is.na(WeightData2)]<-0
  Ej1<-sum(WeightData2$PijLN)
  Ej<-(-((1/log(nr))*Ej1))
  return(Ej)
}

