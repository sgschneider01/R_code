plotAffy <- function (egid, mat) {
# Plots probe locations (affy or mapped)

#  x.max<-round(max(unlist(locs))/100,0)*100
x.max<-max(mat)
  pretty<-rainbow(nrow(mat))
  plot(1:x.max, rep(0,x.max), type="l", ylim=c(0,1), las=3, xlab="Transcript Location", main=egid)
#xaxp=c(0,x.max,x.max/500), 
  #plot all the probes
  for (i in 1:nrow(mat)) {
     ps <- as.numeric(mat[i,])
     ps <- ps[which(ps>0)]
     if (length(ps) > 0) sap <- sapply(ps, function (x)    
                           lines(rep(x,2),c(0,.1),col=pretty[i],type="l"))
  }
}
