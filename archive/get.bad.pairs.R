get.bad.pairs <- function (x, r.sig, method="pearson") {
  # df is a dataframe with k columns for each probe set and n rows for each chip

  cors <- get.pairwise.corr(x,r.sig,method)
  bad.c <- setdiff(1:nc, 
                   unique(unlist(c(cors[which(cors[,4]>0),1:2]))))
  if (length(bad.c)==0) {cat("All pairs are good.\n"); return(NULL)}
  return(colnames(df)[bad.c])
 
}

