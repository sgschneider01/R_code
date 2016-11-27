plotProbeSets <- function (sp,gene_id=NULL,symbol=NULL) {
   if (is.null(gene_id) && is.null(symbol)) {
       stop("You must specify either a gene id or symbol.")
   }

   org.pkg <- do.eval(paste("org",sp,"egSYMBOL",sep='.'))  # eg org.Hs.egSYMBOL
   if (is.null(gene_id)) {
      gene_id <- mappedLkeys(subset(org.pkg, Rkeys=symbol))
   }
   if (is.null(symbol)) {
      symbol <- get(gene_id,org.pkg)
   }
   if (is.numeric(gene_id)) gene_id <- as.character(gene_id)

   id <-paste(symbol," (",gene_id,")",sep='')

   gen.locs <- getGeneLocs(gene_id,sp)
   gen.locs <- sapply(gen.locs,function(x)subset(x,!is.na(x)))
   pretty   <- rainbow(length(gen.locs))
   ps.ids   <- names(gen.locs)

   exon.tab <- getExonTable(gene_id)
   if (is.null(exon.tab)) {
       exon.tab <- as.numeric(getEnds(gene_id))
       exon.tab <- exon.tab-exon.tab[1]+1
       exon.tab <- list(no_exon_info_found=list(cds=exon.tab,exons=NA))
   }

   x.max <- ceiling(max(as.numeric(unlist(exon.tab)),na.rm=T)/100)*100
   x.min <- 1

   sap <- unlist(sapply(gen.locs, function(x) nchar(names(x))))
   gl <- unlist(gen.locs)
   pretty2 <- rep(pretty,sapply(gen.locs,length))

   # create the axes
   plot (x.min:x.max, rep(0,x.max-x.min+1), type="l", ylim=c(-.5,.5), las=3, yaxt="n", xlab="Gene Position (nucleotides)", ylab="", main=id)

   #plot all the probes
   for (i in 1:length(gl)) {
      lines(rep(gl[i],2),c(0,.005*sap[i]), col=pretty2[i], type="l")
   }
   lbls <- paste(1:length(ps.ids),ps.ids,sep=' ')
   legend("topleft",lbls,lty=1,bty="n",col=pretty,cex=.7)

   if (!is.null(exon.tab)) {
      n <- length(exon.tab)  # one per *transcript*
      grays <- gray(seq(0,.7,by=.7/n))
      for (i in 1:n) {
         #plot the start and stop positions (coding sequence)
         cds<-exon.tab[[i]]$cds
         points(cds,rep(-1*i/25,2),pch="|",col=c("green","red"),cex=.7)
         points(cds,rep(-1*i/25,2),pch=c(">","*"),col=c("green","red"),cex=.7)

         #plot the exons
         exons<-exon.tab[[i]]$exons
         if (!is.null(nrow(exons))) {
            for (e in 1:nrow(exons)) {
              lines(exons[e,],rep(-1*i/25,2),type="l",lwd=8,col=grays[i],lend=1)
            }
         } else {
            lines(cds,rep(-1*i/25,2),type="l",lwd=8,col="black",lend=1)
         }
      }
      id.ex<-gsub("none","",names(exon.tab))
      legend("bottom",id.ex,bty="n",fill=grays,cex=.7)
   }  else { cat("No exon information available.\n") }

}

