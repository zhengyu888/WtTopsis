#' zj2max
#' Intermediate Data conversion
#' @param x The Data that needs to be maximized.
#' @param xbest An optimal value vector of intermediate numerical values,other types of indicators are marked as NA.
#' @param i Index column.
#'
#' @return Index column maximized
zj2max<-function(x,xbest,i){
  i=i
  xbest=xbest
  sampleData2<-x
  sampleData31<-sampleData2[,i]
  sampleData32<-1-((abs(sampleData31-xbest[i-1]))/(max(abs(sampleData31-xbest[i-1]))))
  sampleData33<-data.frame(sampleData2[,1],sampleData32)
  names(sampleData33)[1]<-names(sampleData2)[1]
  names(sampleData33)[2]<-names(sampleData2)[i]
  transDatacol<-sampleData33
  return(transDatacol)
}
