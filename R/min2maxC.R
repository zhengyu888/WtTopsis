#' min2maxC
#' negative Data conversion use Maximum subtraction method
#' @param x The Data that needs to be maximized.
#' @param i Index column.
#'
#' @return Index column maximized
min2maxC<-function(x,i){
  sampleData2<-x
  i=i
  sampleData3<-sampleData2[,c(1,i)]
  sampleData3$FZZH<- max(sampleData3[,2])-sampleData3[,2]
  sampleData4<-sampleData3[,c(1,3)]
  transDatacol<-sampleData4
  names(transDatacol)[1] <- names(sampleData2[1])
  names(transDatacol)[2] <- names(sampleData2[i])
  return(transDatacol)
}
