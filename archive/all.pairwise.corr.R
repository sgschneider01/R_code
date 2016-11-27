all.pairwise.corr <- function (x, r.sig, method="pearson") {
  cors <- get.pairwise.corr(x,r.sig,method)

  return (all(cors[,4]>0))
}

