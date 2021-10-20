#' max2max
#' positive Data conversion.
#' @param x The Data that needs to be maximized.
#' @param i Index column.
#'
#' @return Index column maximized
max2max<-function(x,i){
  sampleData2<-x
  i=i
  sampleData3<-sampleData2[,c(1,i)]
  transDatacol<-sampleData3
  return(transDatacol)
}
