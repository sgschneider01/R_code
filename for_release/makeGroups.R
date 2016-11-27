require(genefilter) 

makeGroups <- function (eset, ann.table=NULL, filtered=TRUE, alpha=.01) {
cat("Please be patient, this make take a few minutes.\n")

  if (!filtered)  eset <- filterGenes(eset)
  if (is.null(ann.table))  {
    if (length(annotation(eset))==0) stop("The platform is not specified in the ExpressionSet object, please provide an ann.table.")
    ann.table <- makeAnnTable(annotation(eset))
  }
  exprs.eset <- exprs(eset)
  nc <- ncol(exprs.eset)
  r.crit <- getRcrit(alpha,nc)
  w.crit <- (1+r.crit)/2

  ann.table <- subset(ann.table, ann.table$probe_id %in% rownames(exprs.eset))
  multiples <- subset(ann.table,ann.table$Freq>1)
  new.ps.num <- by(multiples,multiples$gene_id,function(ss) {
    eset.rows <- subset(exprs.eset, rownames(exprs.eset) %in% ss$probe_id)
    if (nrow(eset.rows)>1) {
      mat <- cor(t(eset.rows),method="spearman")
      w   <- mean(mat,na.rm=TRUE) 
      if (w >= w.crit) {
        ss$ps.num <- paste(ss$ps.num,collapse='_')
      } else {
        sg <- getSubGraphs(mat,alpha,nc,w.crit)  # returns names not indices
        for (s in sg) {
          if (!is.numeric(s))  s <- sort(match(s,ss$probe_id))
          ss$ps.num[s] <- paste(ss$ps.num[s],collapse="_")
        }
      } 
    }
    ss$ps.num
  })
  multiples$ps.num <- unlist(new.ps.num,use.names=F)
  singles <- subset(ann.table,ann.table$Freq==1)
  multiples <- rbind(multiples,singles)
  new.eg.ids <- paste(multiples$gene_id,multiples$ps.num,sep='.')
  new.eg.ids <- paste(new.eg.ids,multiples$Freq,sep='.')  
  new.ids <- data.frame(probe_id=multiples$probe_id, new.gene_id=new.eg.ids)
  new.ids
}