# P values calculatation
# daily 1S-KS test p values
# Daily 2S-Bootstrapped KS test p values compared to previous day
# Daily 2S-LR test p values
# Input: data object results after preparation
# Output: A data frame with columns being
#         "day","Bs.1ksPvalue", "X1ksPvalue","X1ksStat","NoOfReadings","Bs.2ksPvalue","X2ksPvalue","X2ksStat","X2lrPval"
# This function could save the p values in a separate file incase for further use.
# Changes made by Ning Yilin on 2016-01-06:
# - Changed colour scheme for plots.
# - Colours used are selected from website ColorBrewer (http://colorbrewer2.org/)
#   colVec <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d')

#' @title Calculating KS p-values
#' @description This function performs one-sample Kolmogorov-Smirnov test of the distribution of
#' daily BG timings against uniformation distribution, and two-sample Kolmogorov-Smirnov test for
#' the distributions of BG timings from two consecutive days, and provides p-values. 
#' @param dat A \emph{data.table} prepared by \code{datapreparation}
#' @return A \emph{data.frame} with columns being
#'   \itemize{
#'     \item{day} The column indicates day counts
#'     \item{Bs.1ksPvalue} The bootstrapped One-Sample Kolmogorov-Smirnov test
#'     p-values
#'     \item{X1ksPval} The exact One-Sample Kolmogorov-Smirnov test p-values
#'     \item{X1ksStat} The test statistics of One-Sample Kolmogorov-Smirnov
#'     test
#'     \item{Bs.2ksPvalue} The bootstrapped Two-Sample Kolmogorov-Smirnov test
#'     p-values
#'     \item{X2ksPval} The exact Two-Sample Kolmogorov-Smirnov test p-values
#'     \item{X2ksStat} The test statistics of Two-Sample Kolmogorov-Smirnov
#'     testtrapped One-Sample Kolmogorov-Smirnov test p-values
#'     \item{X2lrPval} The exact Two-Sample Likilihood Ratio test p-values
#'     \item{NoOfReadings} The number of measurements taken within that day
#'   }
#'
#' @author Tan Chuen Seng, Chen Ying
#' @export
pCalfunc <- function(dat){
   dat[, weekd := as.numeric(format(.SD$RESULT.DATE, "%u"))] # give the day of the week in 1 to 7
  dat[, weekn := as.numeric(format(.SD$RESULT.DATE, "%W"))] # give the week of the year from 1 to 53
  dat[, mond := as.numeric(format(.SD$RESULT.DATE, "%e"))] # give the day of the month from 01 to 31
  dat[, yday := as.numeric(format(.SD$RESULT.DATE, "%j"))] # give the day of the year, a number in 1 to 366

  #1-sample ks test
  new.dat = dat[,list(RESULT.DATE=.SD$RESULT.DATE,weekn=.SD$weekn,yday=.SD$yday,mond=.SD$mond,weekd=.SD$weekd,hour=.SD$hour)]##*##
  setkey(new.dat,yday)
  all.list=list()
  count=0
  yday1=as.numeric(format(min(dat$RESULT.DATE),"%j"))
  yday2=as.numeric(format(max(dat$RESULT.DATE),"%j"))
  dayno=yday1:yday2 #specific day of the year
  for(i in 1:length(dayno)){
    count=count+1
    id=which(new.dat$yday==dayno[i])
    tmp=new.dat[id,]
    if(nrow(tmp)>0){
      all.list[[count]]=tmp
    }else{
      all.list[[count]]=matrix(NA,nrow=1,ncol=7)
    }
  }

  p.val.1ks=NULL

  for(i in 1:length(all.list)){
    ##TCS: Modification
    #if(!is.na(all.list[[i]])){
    if(!((nrow(all.list[[i]])==1)&(all(is.na(all.list[[i]]))))){
      a<-all.list[[i]]$hour
      out <-try(ks.test(a,"punif",min=0,max=24,exact=FALSE),silent=TRUE)
      if(!inherits(out, "try-error")){
        tmp=out$statistic
        tmp1<-out$p.value
      }else{
        tmp<-NA
        tmp1=NA
      }
      p.val.1ks=rbind(p.val.1ks,c(tmp,tmp1,nrow(all.list[[i]])))
    }else{
      p.val.1ks=rbind(p.val.1ks,c(NA,NA,0))
    }}
  out1 <-p.val.1ks

  p.val.2ks=NULL
  p.val.2ks=c(NA,NA,NA)
  for(i in 2:length(all.list)){
    test1 = !((nrow(all.list[[i]])==1)&(all(is.na(all.list[[i]]))))
    test2 = !((nrow(all.list[[i-1]])==1)&(all(is.na(all.list[[i-1]]))))
    if(all(test1, test2)){
      day2=as.data.frame(all.list[i])
      day1=as.data.frame(all.list[i-1])
      out<-ks.boot(as.numeric(day1$hour),as.numeric(day2$hour),nboots=1000)
      tmp=out$ks.boot.pvalue
      tmp1=out$ks$p.value
      tmp2=out$ks$statistic
      p.val.2ks=rbind(p.val.2ks,c(tmp,tmp1,tmp2))
    }else{
      p.val.2ks=rbind(p.val.2ks,c(NA,NA,NA))
    }
  }
  out2 <-p.val.2ks

  dat=p.val.1ks[,3]
  p.val.2lr=NULL
  tmp1=factor(c(0,1))
  p.val.2lr=NA
  for(j in 2:length(dat)){
    ## all ##
    if(all(dat[c(j,j-1)]>0)){
      tmp=dat[c(j,j-1)]
      fit1=glm(tmp~1,family="poisson")
      fit=glm(tmp~tmp1,family="poisson")
      p.val.2lr=c(p.val.2lr,lrtest(fit,fit1)[[5]][2])
    }else{
      p.val.2lr=c(p.val.2lr,NA)
    }
  }
  out3 <-p.val.2lr
  tmp <- (dayno-yday1+1)
  out.table <- cbind(tmp,out1,out2,out3)
  out.table=out.table[,c(1:3,5:8,4)]
  rownames(out.table) <-1:length(all.list)
  colnames(out.table) <- c("day", "X1ksStat","X1ksPvalue","Bs.2ksPvalue","X2ksPvalue","X2ksStat","X2lrPval","NoOfReadings")
  return(out.table)
}





