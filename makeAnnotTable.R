makeAnnTable <- function (exp.plat) {
   exp.plat <- as.character(exp.plat)  
   require(paste(exp.plat,"db",sep='.'),character.only=T)
   ep <- paste(exp.plat,"ENTREZREANNOTATED",sep="")
   if (exists(ep)) {
     xx <- toTable(do.eval(ep))
     colnames(xx) <- c("probe_id","gene_id")
   } else {
     xx <- toTable(do.eval(paste(exp.plat,"ENTREZID",sep="")))
   }
   xx <- cbind(xx, ps.num=1)
   xy <- subset(xx,!duplicated(xx$gene_id))
   dups <- subset(xx,duplicated(xx$gene_id))
   for (i in 2:max(table(xx$gene_id))) { 
      ones <- subset(dups,!duplicated(dups$gene_id))
      ones$ps.num <- i
      dups <- subset(dups,duplicated(dups$gene_id))
      xy <- rbind(xy,ones)
   }
   xy <- xy[order(xy$gene_id),]
   tab <- as.data.frame(table(xy$gene_id))
   xy <- merge(xy, tab, by.x="gene_id", by.y=1)
   return(xy)
}
