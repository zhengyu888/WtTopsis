#' DataNormal
#' Normalize the maximized data set.
#' @param data A maximized data set
#'
#' @return A Normalized data set
#' @export DataNormal
#'
#' @examples
#' \dontrun{
#' DataNormal(data2max(sampleData))
#' }
DataNormal<-function(data){
  Normaldata<-data
  nc<-ncol(Normaldata)
  DataNormal1<-data.frame()
  DataNormal1<-data.frame(Normaldata[,1])
  names(DataNormal1)[1]<-names(Normaldata)[1]
  for (i in 2:nc){
    DataNormalcol<-DataNormalpre(Normaldata,i)
    DataNormal1<-merge(DataNormal1,DataNormalcol)[order(DataNormal1[,1]),]
  }
  return(DataNormal1)
}
