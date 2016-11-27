plotExons <- function (exon.tab) {
  n <- length(exon.tab)
  grays<-rep(c("black","gray"),n)
  for (i in 1:n) {
      #plot the exons
      exons<-exon.tab[[i]]$exons
      for (e in 1:nrow(exons)) {
        lines(exons[e,],rep(-1*i/25,2),type="l",lwd=8,col=grays[i],lend=1) 
      }
      #plot the start and stop positions (coding sequence)
      cds<-exon.tab[[i]]$cds
      points(cds,rep(-1*i/25,2),pch="|",col=c("green","red"),cex=.7)
      points(cds,rep(-1*i/25,2),pch=c(">","*"),col=c("green","red"),cex=.7)
  }
  id.ex<-paste(gsub(" .*: "," (",names(exon.tab)) ," exons)",sep='')
  legend("bottom",id.ex,bty="n",fill=grays,cex=.7)
}
