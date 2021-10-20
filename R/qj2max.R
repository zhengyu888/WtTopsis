#' qj2max
#' Interval Data conversion
#' @param x The Data that needs to be maximized.
#' @param qlow An interval lower bound vector with interval numeric value forward,other types of indicators are marked as NA.
#' @param qup An interval upper bound vector of the interval value maximize,other types of indicators are marked as NA.
#' @param i Index column.
#'
#' @return Index column maximized
qj2max<-function(x,qlow, qup,i){
  sampleData3<-x
  i=i
  a<-qlow[i-1]
  b<-qup[i-1]
  maxdata=max(sampleData3[,i])
  mindata=min(sampleData3[,i])
  sampleData31<-subset(sampleData3,sampleData3[,i]<a)
  sampleData32<-subset(sampleData3,sampleData3[,i]>=a & sampleData3[,i]<=b)
  sampleData33<-subset(sampleData3,sampleData3[,i]>b )
  sampleData31$QJzhz<-1-((a-sampleData31[,i])/(max(a-mindata,maxdata-b)))
  sampleData32$QJzhz<-1
  sampleData33$QJzhz<-1-((sampleData33[,i]-b)/(max(a-mindata,maxdata-b)))
  sampleData34<-rbind(sampleData31,sampleData32,sampleData33)
  sampleData35<-sampleData34[order(sampleData34[,1]),]
  sampleData36<-sampleData35$QJzhz
  sampleData37<-sampleData35[,1]
  transDatacol<-data.frame(sampleData37,sampleData36)
  names(transDatacol)[1] <- names(sampleData3[1])
  names(transDatacol)[2] <- names(sampleData3[i])
  return(transDatacol)
}
