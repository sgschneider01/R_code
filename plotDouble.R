source("~/Desktop/research/notes/R_code/getAffyLocs.R")
source("~/Desktop/research/notes/R_code/getTxLocs.R")
source("~/Desktop/research/notes/R_code/getExons.R")
source("~/Desktop/research/notes/R_code/getSTS.R")
source("~/Desktop/research/notes/R_code/plotExons.R")


plotDouble <- function (egid,sp) {
# Plots probe locations (affy and mapped)
  if (is.numeric(egid)) egid <- as.character(egid)
  mat1 <- getAffyLocs(egid,sp)
  mat2 <- getTxLocs(egid,sp)

  x.max <- round(max(mat1,mat2)/100,0)*100

  sym <- switch(sp, "Mm"    =  get(egid,org.Mm.egSYMBOL),
                    "mouse" =  get(egid,org.Mm.egSYMBOL),
                    "Hs"    =  get(egid,org.Hs.egSYMBOL),
                    "human" =  get(egid,org.Hs.egSYMBOL)  )
  id <-paste(sym," (",egid,")",sep='')

  pretty <- rainbow(nrow(mat1))
  # create the axes
  plot (1:x.max, rep(0,x.max), type="l", ylim=c(-.2,1.5), las=3,
         xlab="Transcript Position", ylab="", main=id)
  legend("topright",rownames(mat1),lty=1,bty="n",col=pretty)

  #plot all the probes
  for (i in 1:nrow(mat1)) {
     ps <- as.numeric(mat1[i,])
     ps <- ps[which(ps>0)]
     if (length(ps) > 0) sap <- sapply(ps, function (x)    
                           lines(rep(x,2),c(0,.1),col=pretty[i],type="l"))
  }
  for (i in 1:nrow(mat2)) {
     ps <- as.numeric(mat2[i,])
     ps <- ps[which(ps>0)]
     if (length(ps) > 0) sap <- sapply(ps, function (x)    
                           lines(rep(x,2),c(-.1,0),col=pretty[i],type="l"))
  }

  plotExons(egid,sp)
}
