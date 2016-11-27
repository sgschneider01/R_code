source("~/Desktop/research/notes/R_code/getGeneLocs.R")
source("~/Desktop/research/notes/R_code/plotAltExons.R")
source("~/Desktop/research/notes/R_code/parseExonTable.R")
source("~/Desktop/research/notes/R_code/getSeq.R")

plotGenZoom <- function (egid,platform,ss=NULL) {
  if (is.numeric(egid)) egid <- as.character(egid)

  gen.locs <- getGeneLocs(egid,platform) 

  org.pkg <- do.eval(paste(platform,"ORGPKG",sep=''))
  sym <- get(egid,do.eval(paste(org.pkg,"SYMBOL",sep='')))

  id <-paste(sym," (",egid,")",sep='')

  pretty <- rainbow(nrow(gen.locs))
  lbls   <- rownames(gen.locs)

  exons <- parseExonTable(egid)
  if (!is.null(ss)) { # if we only want to print a small section
     gen.locs  <- gen.locs[ss,]
     x.min <- round(min(subset(as.vector(gen.locs), as.vector(gen.locs)>0))/100,0) *100-100
     x.max <- round(max(gen.locs)/100,0)*100+100
     lbls  <- lbls[ss]
     pretty <- pretty[ss]
  } else {
     x.max <- ceiling(max(as.numeric(unlist(exons)))/100)*100
     x.min <- 1
  }

  # create the axes
  plot (x.min:x.max, rep(0,x.max-x.min+1), type="l", ylim=c(-.5,1), las=3, yaxt="n", xlab="Gene Position", main=id)

  #plot all the probes
  ps.mapped<-NULL
  for (i in 1:nrow(gen.locs)) {
     ps <- as.numeric(gen.locs[i,])
     ps <- subset(ps,ps>0)
     ps.mapped[i] <- length(ps)
     sap <- sapply(ps, function (x) {  # assign this to sap so it doesn't print
                        lines(rep(x,2),c(0,.1),col=pretty[i],type="l") } )
  }

  ps.mapped <- sapply(ps.mapped,function(t)paste(" (",t,"/11)",sep=''))

  lbls <- paste(lbls,ps.mapped,ps.kept,sep='')
  legend("topleft",lbls,lty=1,bty="n",col=pretty,cex=.7)

  if (!is.null(exons)) plotExons(exons)
  else cat("No exon information available.\n")
}
