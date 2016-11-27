plotProbeSets <- function (gene_id=NULL,symbol=NULL,platform,probe.sets=NULL, new.ids=NULL) {
  if (is.null(gene_id) & is.null(symbol)) {
     stop("You must specify either a gene id or symbol.")
  }
  if (is.null(platform)) {
     stop("You must specify an Affymetrix platform.")
  }

  org.pkg <- do.eval(paste(platform,"ORGPKG",sep=''))

  if (is.null(gene_id)) {
    gene_id <- mappedLkeys(subset(org.Mm.egSYMBOL,Rkeys=symbol))
  }
  if (is.null(symbol)) {
    symbol <- get(gene_id,do.eval(paste(org.pkg,"SYMBOL",sep='')))
  }

  if (is.numeric(gene_id)) gene_id <- as.character(gene_id)

  id <-paste(symbol," (",gene_id,")",sep='')

  gen.locs <- getGeneLocs(gene_id,platform) 
  pretty <- rainbow(nrow(gen.locs))
  lbls   <- rownames(gen.locs)

  exon.tab <- getExonTable(gene_id)

  if (!is.null(probe.sets)) { # if we only want to print a small section
     gen.locs  <- gen.locs[probe.sets,]
     x.min <- round(min(subset(as.vector(gen.locs), as.vector(gen.locs)>0))/100,0) *100-100
     x.max <- round(max(gen.locs)/100,0)*100+100
     lbls  <- lbls[probe.sets]
     pretty <- pretty[probe.sets]
  } else {
     x.max <- ceiling(max(as.numeric(unlist(gen.locs)))/100)*100
     x.min <- 1
  }

  # create the axes
  plot (x.min:x.max, rep(0,x.max-x.min+1), type="l", ylim=c(-.5,.5), las=3, yaxt="n", xlab="Gene Position", ylab="", main=id)

  #plot all the probes
  ps.mapped<-NULL
  for (i in 1:nrow(gen.locs)) {
     ps <- as.numeric(gen.locs[i,])
     ps <- subset(ps,ps>0)
     ps.mapped[i] <- length(ps)
     for (x in ps) {
        lines(rep(x,2),c(0,.1),col=pretty[i],type="l")
     }
  }

  ps.mapped <- sapply(ps.mapped,function(t)paste(" (",t,"/11)",sep=''))

  lbls <- paste(1:length(lbls),lbls,ps.mapped,sep=' ')
  legend("topleft",lbls,lty=1,bty="n",col=pretty,cex=.7)

  if (!is.null(exon.tab)) {
    n <- length(exon.tab)  # one per *transcript*
    grays <- gray(seq(0,.7,by=.7/n))
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
  }  else {cat("No exon information available.\n")}
}
