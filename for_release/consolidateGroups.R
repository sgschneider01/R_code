consolidateGroups <- function (object, new.ids)
{
  m <- object$coefficients  
  m <- subset(m,rownames(m) %in% new.ids$probe_id)  
  new.ids <- subset(new.ids, new.ids$probe_id %in% rownames(m))
  m <- data.frame(m[as.character(new.ids$probe_id),])
  m <- apply(m,2,function(x) tapply(x,as.character(new.ids$new.gene_id),mean))

  p <- object$p.value
  p <- subset(p,rownames(p) %in% new.ids$probe_id)  
  p <- data.frame(p[as.character(new.ids$probe_id),])
  p <- apply(p,2,function(x) tapply(x,as.character(new.ids$new.gene_id), fisher.p))

  object$coefficients <- m
  object$p.value <- p
  return(object)
}
