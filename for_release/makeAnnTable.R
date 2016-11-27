makeAnnTable <- function (exp.plat) {
  require(paste(exp.plat,"db",sep='.'),character.only=T)
   xx <- toTable(do.eval(paste(exp.plat,"ENTREZID",sep="")))
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

# Produces a table similar to the following:
#     gene_id     probe_id ps.num Freq
#1  100009600   1446427_at      1    1
#2     100012 1436720_s_at      1    2
#3     100012 1436721_x_at      2    2
#4     100017   1424378_at      1    3
#5     100017   1457162_at      2    3
