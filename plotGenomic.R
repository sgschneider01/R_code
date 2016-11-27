source("~/Desktop/research/notes/R_code/getGenomicLocs.R")
source("~/Desktop/research/notes/R_code/plotAltExons.R")


plotGenomic <- function (egid,sp,new.id.tab=NULL) {
# Plots probe locations (affy and mapped)
  if (is.numeric(egid)) egid <- as.character(egid)

  mat2 <- getGenomicLocs(egid,sp)

  x.max <- round(max(mat2)/100,0)*100

  sym <- switch(sp, "Mm"    =  get(egid,org.Mm.egSYMBOL),
                    "Hs"    =  get(egid,org.Hs.egSYMBOL) )
  id <-paste(sym," (",egid,")",sep='')

  if (!is.null(new.id.tab)) {
    new.ids <- merge(new.id.tab,rownames(mat2),by=1,all.y=T)
    new.ids[which(is.na(new.ids[,2])),2] <- paste(egid,0,sep='.')
  } else {
    new.ids <- cbind(rownames(mat2),1:nrow(mat2))
  }
  
  facts <- as.numeric(factor(new.ids[,2]))
  pretty <- rainbow(max(facts))   #  pretty <- rainbow(nrow(mat2))
  pretty <- pretty[facts]

  # create the axes
  plot (1:x.max, rep(0,x.max), type="l", ylim=c(0,1), las=3, yaxp=c(0,0,1), 
        xlab="Transcript Position", ylab="", main=id)

  lbls <- paste(new.ids[,1]," (", new.ids[,2],")",sep='')
  legend("topright",lbls,lty=1,bty="n",col=pretty)
  
  for (i in 1:nrow(mat2)) {
     ps <- as.numeric(mat2[i,])
     for (j in 1:length(ps)) {
       if (ps[j]<0) 
         lines(rep(i*1000+j*100,2),c(.5,.6),col=pretty[i],type="l")
       else 
         lines(rep(ps[j],2),c(0,.1),col=pretty[i],type="l")
     }
  }
  plotAltExons(egid)   #plotExonsIntrons(egid,sp)
}
