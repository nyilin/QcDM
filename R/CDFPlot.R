#' @title Cumulative distribution function plot
#' @description Plot the cumulative distribution of daily blood glucose timings
#' @param dat, as in \code{\link{PerformExclusion}}.
#' @author Ying Chen, Chuen Seng Tan
#' @export
CDFPlot <- function(dat){
  par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(4,2,1,2)+0.5)
  nn=c(1,2,23,24)
  for(i in 1:4){
    list=tmp[tmp$LOCATION==ward[nn[i]],]
    plot(NA,type="n",xlim=c(0,24),ylim=c(0,1),xaxt='n',yaxt='n',
         ylab="F(t)",xlab="")
    title(paste(LETTERS[i]))
    if(i %in% c(3,4)){
      axis(1,at=seq(0,24,by=2),labels=c("12am",paste(seq(2,10,by=2),"am",sep=''),"12noon",paste(seq(2,10,by=2),"pm",sep=''),"12am"))
      title(paste(LETTERS[i]),xlab="Observed Glucose Timings")
    }
    if(i %in% c(1,3)){
      axis(2,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2))
    }

    abline(v=c(8,12,18,22),col="black",lty=2,lwd=1)
    for(j in 1:length(days)){
      list_tmp=list[list$day==days[j],]
      if(nrow(list_tmp)>0){
        list_tmp=data.frame(list_tmp)
        out=CDF(list_tmp$hour)
        lines(out[,2]~out[,1],col="grey21",type="s",lwd=0.15)
        segments(0,0,out[1,1],0,col="grey21",lwd=0.15)
        segments(out[1,1],0,out[1,1],out[1,2],col="grey21",lwd=0.15)
        segments(out[nrow(out),1],1,24,1,col="grey21",lwd=0.15)
      }
    }
    list=data.frame(list)
    out=CDF(list$hour)
    lines(out[,2]~out[,1],col="black",type="s",lwd=2)
    segments(0,0,out[1,1],0,col="black",lwd=2)# make it start from 0
    segments(out[1,1],0,out[1,1],out[1,2],col="black",lwd=2)# connect 0 to out[,1]
    segments(out[nrow(out),1],1,24,1,col="black",lwd=2)
    abline(a=0,b=1/24,col="black",lty=2,lwd=2)
  }
}
