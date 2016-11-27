source("~/Desktop/research/notes/R_code/getTxLocs.R")
source("~/Desktop/research/notes/R_code/getExons.R")
source("~/Desktop/research/notes/R_code/getSTS.R")
source("~/Desktop/research/notes/R_code/plotExons.R")

plotProbesExons <- function (egid,sp) {

  if (is.numeric(egid)) egid <- as.character(egid)
  mat <- getTxLocs(egid,sp)

  switch(sp,
    "Mm"    =  sym <- get(egid,org.Mm.egSYMBOL),
    "mouse" =  sym <- get(egid,org.Mm.egSYMBOL),
    "Hs"    =  sym <- get(egid,org.Hs.egSYMBOL),
    "human" =  sym <- get(egid,org.Hs.egSYMBOL)
  )
  id <-paste(sym," (",egid,")",sep='')

  pretty <- rainbow(nrow(mat))
  # create the axes
  x.max <- round(max(mat)/100,0)*100
  plot (1:x.max, rep(0,x.max), type="l", ylim=c(-.2,1.5), las=3,
        xaxp=c(0,x.max,x.max/100), xlab="Transcript Position", main=id)
  legend("topright",rownames(mat1),lty=1,bty="n",col=pretty)

  #plot all the probes
  for (i in 1:nrow(mat)) {
     ps <- as.numeric(mat[i,])
     ps <- ps[which(ps>0)]
     if (length(ps) > 0) sap <- sapply(ps, function (x)    
                           lines(rep(x,2),c(0,.1),col=pretty[i],type="l"))
  }

  plotExons(egid,sp)
}
