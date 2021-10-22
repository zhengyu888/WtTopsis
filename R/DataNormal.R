#' DataNormal
#' Normalize the maximized data set.
#' @param data A maximized data set
#'
#' @return A Normalized data set
#' @export DataNormal
#'
#' @examples
#' \dontrun{
#' sampleData<-sampleData
#' mth<-c("ZH","ZJ","QJ","ZH","FU","ZH","QJ","FU","FU","ZH")
#' zmth<-c(NA,NA,NA,NA,"CZ",NA,NA,"DS","CZ",NA)
#' xbest<-c(NA,4,NA,NA,NA,NA,NA,NA,NA,NA)
#' qup<-c(NA,NA,5,NA,NA,NA,5,NA,NA,NA)
#' qlow<-c(NA,NA,3,NA,NA,NA,3,NA,NA,NA)
#' maxdata<-data2max(sampleData,mth,zmth,xbest,qlow,qup)
#' DataNormal(maxdata)
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
