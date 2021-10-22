#' WtTopsis
#' Weighted TOPSIS method for multiple-criteria decision making (MCDM) us Normalized data.
#' @param data Normalized data.
#'
#' @return Data set containing D+,D- and C values
#' @export WtTopsisN
#'
#' @examples
#' \donttest{
#' sampleData<-sampleData
#' mth<-c("ZH","ZJ","QJ","ZH","FU","ZH","QJ","FU","FU","ZH")
#' zmth<-c(NA,NA,NA,NA,"CZ",NA,NA,"DS","CZ",NA)
#' xbest<-c(NA,4,NA,NA,NA,NA,NA,NA,NA,NA)
#' qup<-c(NA,NA,5,NA,NA,NA,5,NA,NA,NA)
#' qlow<-c(NA,NA,3,NA,NA,NA,3,NA,NA,NA)
#' maxdata<-data2max(sampleData,mth,zmth,xbest,qlow,qup)
#' normaldata<-DataNormal(maxdata)
#' WtTopsisN(normaldata)
#' }
WtTopsisN<-function(data){
  maxdata<-data
  nc<-ncol(maxdata)
  nr<-nrow(maxdata)
  MAXdata=NULL
  MINdata=NULL
  for (i in 2:nc){
    MAXI<-max(maxdata[,i])
    names(MAXI)<-names(maxdata)[i]
    MINI<-min(maxdata[,i])
    names(MINI)<-names(maxdata)[i]
    MAXdata<-c(MAXdata,MAXI)
    MINdata<-c(MINdata,MINI)
  }
  MaxData<-data.frame(MAXdata)
  MaxData<-t(MaxData)
  MinData<-data.frame(MINdata)
  MinData<-t(MinData)
  BestResult<-rbind(MaxData,MinData)


  Dmax<-data.frame()
  Dmax<-data.frame(maxdata[,1])
  names(Dmax)[1]<-names(maxdata)[1]

  Dmin<-data.frame()
  Dmin<-data.frame(maxdata[,1])
  names(Dmin)[1]<-names(maxdata)[1]

  Wi<-GetWeight(maxdata)


  for (i in 2:nc){
    maxdata1<-maxdata[,c(1,i)]
    maxdata1$Djia<-Wi["weight",i-1]*((maxdata[,i]-BestResult["MAXdata",i-1])**2)
    maxdata2<- maxdata1[,c(1,3)]
    names(maxdata2)[2]<-names(maxdata)[i]
    Dmax<-merge(Dmax,maxdata2)[order(Dmax[,1]),]

    mindata1<-maxdata[,c(1,i)]
    mindata1$Djian<-Wi["weight",i-1]*((maxdata[,i]-BestResult["MINdata",i-1])**2)
    mindata2<- mindata1[,c(1,3)]
    names(mindata2)[2]<-names(maxdata)[i]
    Dmin<-merge(Dmin,mindata2)[order(Dmin[,1]),]
  }

  Dma=NULL
  Dmi=NULL
  for (i in 1:nr){
    Dma1<-sqrt(sum(Dmax[i,c(2:nc)]))
    Dma<-c(Dma,Dma1)

    Dmi1<-sqrt(sum(Dmin[i,c(2:nc)]))
    Dmi<-c(Dmi,Dmi1)

  }

  IDM<-Dmax[,1]
  Dvalue<-data.frame(IDM,Dma,Dmi)

  names(Dvalue)[1]<-names(Dmax)[1]
  names(Dvalue)[2]<-"Dmax"
  names(Dvalue)[3]<-"Dmin"

  Dvalue$Cvalue<-(Dvalue$Dmin)/((Dvalue$Dmax)+(Dvalue$Dmin))
  return(Dvalue)
}
