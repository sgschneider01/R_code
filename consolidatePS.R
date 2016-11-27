source("~/Desktop/research/notes/R_code/fisherP.R")

consolidate.ps <- function (fit3, new.ids) {
  m <- fit3$coefficients
  m2 <- data.frame(m[as.character(new.ids$affy.id),])  # REORDERS IDS, REMOVES NAS
  # need the data.frame() in case there is only one column (ie one contrast)
  m2 <- apply(m2,2,function(x) tapply(x,new.ids$new.eg.id,mean))
  colnames(m2) <- colnames(m)
  fit3$coefficients <- m2

  p <- fit3$p.value
  p2 <- data.frame(p[as.character(new.ids$affy.id),])
  p2 <- apply(p2,2,function(x) tapply(x,new.ids$new.eg.id,fisher.p))
  colnames(p2) <- colnames(p)
  fit3$p.value <- p2

  fit3
}


#  Don't need this section since rewrote decideTests to use sign(m) instead of t
#  t <- fit3$t
#  t <- data.frame(t.val=t[as.character(new.ids$affy.id),])
#  t2 <- apply(t,2,function(x) tapply(x,new.ids$new.eg.id,mean))
#  fit3$t <- t2
#
