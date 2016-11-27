source("~/Desktop/research/notes/R_code/getGenomicLocs.R")
source("~/Desktop/research/notes/R_code/getCustGenLocs.R")
source("~/Desktop/research/notes/R_code/parseExonTable.R")
source("~/Desktop/research/notes/R_code/plotAltExons.R")


plotGenDouble <- function (egid,sp,new.ids) {
# Plots probe locations (mapped, and custom CDF)

  if (is.numeric(egid)) egid <- as.character(egid)

  mat2 <- getGenomicLocs(egid,sp) #  same
  mat3 <- getCustGenLocs(egid,sp) #  just a list - all one "probe set"

  exons <- parseExonTable(egid)
  x.max <- ceiling(max(as.numeric(unlist(exons)))/100)*100
  
  switch(sp,
    "Mm"    =  sym <- get(egid,org.Mm.egSYMBOL),
    "mouse" =  sym <- get(egid,org.Mm.egSYMBOL),
    "Hs"    =  sym <- get(egid,org.Hs.egSYMBOL),
    "human" =  sym <- get(egid,org.Hs.egSYMBOL)
  )
  id <-paste(sym," (",egid,")",sep='')

  new.ids <- merge(new.ids,rownames(mat2),by=1,all.y=T)
  new.ids[which(is.na(new.ids[,2])),2] <- paste(egid,0,sep='.')
  facts <- as.numeric(factor(new.ids[,2]))
  pretty <- rainbow(max(facts))   #  pretty <- rainbow(nrow(mat2))
  pretty <- pretty[facts]

  # create the axes
  plot (1:x.max, rep(0,x.max), type="l", ylim=c(-.2,1), las=3, yaxp=c(-.2,1,1), 
        xlab="Transcript Position", ylab="", main=id)

  #plot all the probes
  ps.mapped<-NULL
  for (i in 1:nrow(mat2)) {
     ps <- as.numeric(mat2[i,])
     ps <- subset(ps,ps>0)
     ps.mapped[i] <- length(ps)
     sap <- sapply(ps, function (x) {  # assign this to sap so it doesn't print
                        lines(rep(x,2),c(0,.1),col=pretty[i],type="l") } )
  }

  #mark the custom cdf probes
  for (ps in mat3) {
     if (ps > 0) {
        points(ps,.1,col="black",pch="v") 
     } 
  }

  ps.mapped <- sapply(ps.mapped,function(t)paste(" (",t,"/11)",sep=''))

  ps.kept <- apply(mat2,1,function(x)sum(!is.na(match(x,mat3))))
  ps.kept <- paste(" (",ps.kept,")",sep='')

  lbls <- paste(new.ids[,1]," (", new.ids[,2],")",sep='')
  lbls <- paste(lbls,ps.mapped,ps.kept,sep='')
  legend("topleft",lbls,lty=1,bty="n",col=pretty,cex=.7)

  plotAltExons(egid,sp)
  
}
