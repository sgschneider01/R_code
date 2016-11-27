plotZoom <- function (egid,sp,ss=NULL) {
# Plots mapped probe locations FOR EACH TRANSCRIPT SEPARATELY
  if (is.numeric(egid)) egid <- as.character(egid)
  txs     <- parseExonTable(egid)  # this is a list
  tx.list <- lapply(txs,genom2tx)
  tx.num  <- length(tx.list)
  tx.nms  <- gsub(" .+","",names(tx.list))
  big.mat <- getTxLocs(egid,sp,tx.nms)
  ps.num  <- nrow(big.mat)/tx.num

  if (tx.num>1) devAskNewPage(ask = TRUE)

  for (j in 1:tx.num) {
    mat2   <- big.mat[((j-1)*ps.num+1):(j*ps.num),]
    pretty <- rainbow(nrow(mat2))
    if (!is.null(ss)) {
      mat2 <- mat2[ss,]
      pretty <- pretty[ss]
    }
    x.min <- round(min(mat2)/100,0)*100-100

    if (max(mat2)>0) {
      x.max <- round(max(mat2)/100,0)*100+100
    } else {
      x.max <- max(unlist(tx.list))
    }

    sym <- switch(sp, "Mm"    =  get(egid,org.Mm.egSYMBOL),
                      "mouse" =  get(egid,org.Mm.egSYMBOL),
                      "Hs"    =  get(egid,org.Hs.egSYMBOL),
                      "human" =  get(egid,org.Hs.egSYMBOL)  )
    id <-paste(sym," (",egid,")",sep='')

    # create the axes
    plot (x.min:x.max, rep(0,x.max-x.min+1),type="l",ylim=c(-.5,1),
          yaxp=c(0,0,1), las=3, ylab="", main=id, xlab="Transcript Position")
    legend("topright",rownames(mat2),lty=1,bty="n",col=pretty,cex=.5)

    #plot all the probes
    for (i in 1:nrow(mat2)) {
       ps <- as.numeric(mat2[i,])
       ps <- ps[which(ps>0)]
       if (length(ps) > 0) {
          sap <- sapply(ps, function (x)
                           lines(rep(x,2),c(0,.1),col=pretty[i],type="l") )
       }
    }
    plotAltTxExons(tx.list[j])
  }
  devAskNewPage(ask = FALSE)
}


plotAltTxExons <- function (tx) {

  #plot the exons
  exons  <- tx[[1]]$exons
  alt.col<- rep(c("black","gray"),nrow(exons))
  for (j in 1:nrow(exons)) {
    lines(exons[j,],rep(0,2),type="l",lwd=5,lend=1,col=alt.col[j]) 
  }

  #plot the start and stop positions (coding sequence)
  cds<-tx[[1]]$cds
  points(cds,rep(-.02,2),pch=24,bg=c("green","red"),cex=.7)
    
  id.ex <- gsub(" .+exons: +([0-9]+) ?.*"," (\\1 exons)",names(tx))
  legend("top",id.ex,lty=1,lwd=5,bty="n",col="black",cex=.7)
}
