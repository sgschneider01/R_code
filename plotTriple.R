source("~/Desktop/research/notes/R_code/getAffyLocs.R")
source("~/Desktop/research/notes/R_code/getCustomLocs.R")
source("~/Desktop/research/notes/R_code/getTxLocs.R")
source("~/Desktop/research/notes/R_code/getExons.R")
source("~/Desktop/research/notes/R_code/getSTS.R")
source("~/Desktop/research/notes/R_code/plotExons.R")

plotTriple <- function (egid,sp) {
# Plots probe locations (affy, mapped, and custom CDF)

  if (is.numeric(egid)) egid <- as.character(egid)
  mat1 <- getAffyLocs(egid,sp)   #  a matrix = one row per probe set
  mat2 <- getTxLocs(egid,sp) #  same
  mat3 <- getCustomLocs(egid,sp) #  just a list - all one "probe set"

  exons <- getExons(egid,sp)
  x.max <- ceiling(max(exons)/100)*100
  
  switch(sp,
    "Mm"    =  sym <- get(egid,org.Mm.egSYMBOL),
    "mouse" =  sym <- get(egid,org.Mm.egSYMBOL),
    "Hs"    =  sym <- get(egid,org.Hs.egSYMBOL),
    "human" =  sym <- get(egid,org.Hs.egSYMBOL)
  )
  id <-paste(sym," (",egid,")",sep='')

  pretty <- rainbow(nrow(mat1))
  # create the axes
  plot (1:x.max, rep(0,x.max), type="l", ylim=c(-.2,1.5), las=3,
        #xaxp=c(0,x.max,x.max/100),
        xlab="Transcript Position", main=id)

  #plot all the probes
  for (i in 1:nrow(mat1)) {
     ps <- as.numeric(mat1[i,])
     ps <- subset(ps,ps>0)
     sap <- sapply(ps, function (x) {
                        lines(rep(x,2),c(0,.1),col=pretty[i],type="l") } )
  }
  ps.mapped<-NULL
  for (i in 1:nrow(mat2)) {
     ps <- as.numeric(mat2[i,])
     ps <- subset(ps,ps>0)
     ps.mapped[i] <- length(ps)
     sap <- sapply(ps, function (x) {  # assign this to sap so it doesn't print
                        lines(rep(x,2),c(-.1,0),col=pretty[i],type="l") } )
  }

  for (ps in mat3) {
     if (ps > 0) {
        points(ps,-.1,col="black",pch="^") 
     } 
  }

  ps.mapped <- sapply(ps.mapped,function(t)paste(" (",t,"/11)",sep=''))
  ps.mapped <- c(ps.mapped,paste(" (",sum(mat3>0),"/",length(mat3),")",sep=''))

  ps.kept <- apply(mat2,1,function(x)sum(!is.na(match(x,mat3))))
  ps.kept <- c(ps.kept, sum(ps.kept))
  ps.kept <- paste(" (",ps.kept,")",sep='')

  labels<-c(rownames(mat1),"custom_cdf")
  labels<-paste(labels,ps.mapped,ps.kept,sep='')
  legend("topright",labels,lty=1,bty="n",col=c(pretty,"black"))

  plot.Exons(exons)
  
}
