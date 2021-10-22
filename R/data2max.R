#' data2max
#' Maximized the data.
#'
#' @param data The Data that needs to be maximized.
#' @param mth A vector describing the type of each indicator.
#' @param zmth A vector describing the method from negative to positive, including DS: reciprocal method, it is not recommended to use when the data has 0, CZ: using the maximum subtraction method, other types of indicators are marked as NA.
#' @param xbest An optimal value vector of intermediate numerical values,other types of indicators are marked as NA.
#' @param qlow An interval lower bound vector with interval numeric value forward,other types of indicators are marked as NA.
#' @param qup An interval upper bound vector of the interval value maximize,other types of indicators are marked as NA.
#'
#' @return A maximized data set
#' @export data2max
#'
#' @examples
#' \donttest{
#' sampleData<-sampleData
#' mth<-c("ZH","ZJ","QJ","ZH","FU","ZH","QJ","FU","FU","ZH")
#' zmth<-c(NA,NA,NA,NA,"CZ",NA,NA,"DS","CZ",NA)
#' xbest<-c(NA,4,NA,NA,NA,NA,NA,NA,NA,NA)
#' qup<-c(NA,NA,5,NA,NA,NA,5,NA,NA,NA)
#' qlow<-c(NA,NA,3,NA,NA,NA,3,NA,NA,NA)
#' data2max(sampleData,mth,zmth,xbest,qlow,qup)
#' }
data2max<-function(data,mth,zmth,xbest,qlow,qup){
  sampleData2=data
  nc<-ncol(sampleData2)
  nr<-nrow(sampleData2)

  transData<-data.frame()
  transData<-data.frame(sampleData2[,1])
  names(transData)[1]<-names(sampleData2)[1]

  mth<-mth
  zmth<-zmth
  xbest<-xbest
  qlow<-qlow
  qup<-qup

  for (i in 2:nc){
    {
      if (mth[i-1]== "ZH")
        transDatacol<-max2max(sampleData2,i)
      else if (mth[i-1]== "FU" & zmth[i-1]=="DS")
        transDatacol<- min2maxD(sampleData2,i)
      else if (mth[i-1]== "FU"& zmth[i-1]=="CZ")
        transDatacol<-min2maxC(sampleData2,i)
      else if (mth[i-1]== "ZJ")
        transDatacol<-zj2max(sampleData2,xbest,i)
      else if (mth[i-1]== "QJ")
        transDatacol<-qj2max(sampleData2,qlow, qup,i)
      else transDatacol<-Errmax(sampleData2,i)
    }
    transData<-merge(transData,transDatacol)[order(transData[,1]),]
  }
  return( transData)
}
