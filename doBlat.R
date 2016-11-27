doBlat <- function (egid,sp,all.chr=FALSE) {

  if (all.chr==TRUE) {
    chr <- switch(sp, "Mm" = c(1:19,"X","Y","M") ,
                    "Hs" = c(1:22,"X","Y","M") )
  } else {
    chr <- switch(sp, "Mm" = mget(egid,org.Mm.egCHR)[[1]],
                    "Hs" = mget(egid,org.Hs.egCHR)[[1]] )
  }

  wd <- switch(sp, "Mm" = "~/Desktop/blatSuite.34/mmu/",
                   "Hs" = "~/Desktop/blatSuite.34/hsa/" )
  setwd(wd)
  affx <- switch(sp, "Mm" = "mouse4302probe" ,
                     "Hs" = "hgu133plus2probe"  )
  ps   <- switch(sp, 
                "Mm" = toTable(subset(mouse4302ENTREZID,Rkeys=egid))[,1],
                "Hs" = toTable(subset(hgu133plus2ENTREZID,Rkeys=egid))[,1] )


  ps <- subset(ps, grepl("^[0-9]+_",ps))  # eliminate control probes

  seqs <- subset(do.eval(affx)[,1],do.eval(affx)$Probe.Set.Name %in% ps)

  seqs <- cbind(paste(">seq",1:length(seqs),sep=''),seqs)
  write.table(seqs,file=paste(egid,"fa",sep='.'), quote=F, sep="\n", col.names=F, row.names=F)

  for (i in chr ) {
    cmd <- paste("blat -stepSize=5 -fine -repMatch=1000000 -minScore=25 -minIdentity=25 mm9chr", i, ".fa ", paste(egid,"fa",sep='.'), " tmp.psl", sep='')
    system(cmd)
    system(paste("cat tmp.psl >> ",egid,".psl",sep=''))
  }
  
  
}