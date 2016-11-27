all.but.pairwise.corr <- function (x, r.sig) {
# for a single pair of probesets, return T/F if removal of any one point moves r above r.sig
  # df is a dataframe with k columns for each probe set and n rows for each chip
  if (!(is.data.frame(x) || is.matrix(x))) {
    df <- get.rows(x)
  } else {
    df <- x
  }
  npairs <- choose(ncol(df),2)
  cors   <- matrix(nrow=nrow(df)+1, ncol=npairs)
  pairs  <- matrix(ncol=2, nrow=npairs)

  # iterate over the rows of the dataframe (chips), leaving one out each time
  #  (last iteration keeps all rows)
  for (i in 1:(nrow(df)+1)) { 
    l<-1
    for (j in 1:(ncol(df)-1)) {
      for (k in 2:ncol(df)) {
        if (j<k) {
          r <- cor.test(df[-i,j],df[-i,k])
          cors[i,l] <- cor.test(df[-i,j],df[-i,k])$estimate
          l <- l+1
        }
      }
    }
  }

#  bad.c <- setdiff(1:ncol(df), 
#                   unique(unlist(c(pairs[which(apply(cors,2,all)),]))))
#  bad.r <- which(apply(cors,1,all)) 
#  list(bad.rows=bad.r, bad.cols=bad.c, cors=cors)
return (cors)
}

