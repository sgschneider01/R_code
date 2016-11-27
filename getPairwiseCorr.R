get.all.pairwise <- function (x, r.sig, method="spearman") {
  cors <- get.pairwise.corr(x,r.sig,method)
  return (all(cors[,4]>0))
}

get.bad.ps <- function (x, r.sig, method="spearman") {
  cors <- get.pairwise.matrix(x)
  bads <-which(apply(cors,1,function(x){all(x<r.sig)}))
  if (length(bads)==0) {cat("All pairs are good.\n"); return(NULL)}
  return(bads)
}

get.corrs <- function (x, r.sig, method="spearman") {
  cors <- get.pairwise.corr(x,r.sig,method)
  return (cors[,3])
}

get.pairwise.corr <- function (g, f=NULL, method="spearman") {
  # df is a dataframe with k columns for each probe set and n rows for each chip
  if (!(is.data.frame(g) || is.matrix(g))) {
     df <- get.rows.as.cols(g,f)
  } else {
     df <- g
  }
  nc <- ncol(df)
  npairs <- choose(nc,2)
  cors   <- matrix(nrow=npairs,ncol=3)

  i <- 1
  for (j in 1:(nc-1)) {
     for (k in 2:nc) {
        if (j<k) {
          r<- cor.test(df[,j],df[,k],method=method)$estimate
          cors[i,] <- c(j,k,r)
          i <- i + 1
        }
      }
  }
  return(cors)
}


getPairwiseR <- function (g, f=NULL, method="spearman") {
  # df is a dataframe with k columns for each probe set and n rows for each chip
  if (!(is.data.frame(g) || is.matrix(g))) {
     df <- get.rows.as.cols(g,f)
  } else {
     df <- g
  }
  nodes  <- as.character(colnames(df))
  nc     <- ncol(df)
  npairs <- choose(nc,2)
  cors   <- NULL

  for (j in 1:(nc-1)) {
     for (k in 2:nc) {
        if (j<k) {
          r <- cor.test(df[,j],df[,k],method=method)$estimate
          cors <- data.frame(rbind(cors,c(node1=nodes[j],node2=nodes[k],r)), stringsAsFactors=F)
        }
      }
  }
  return(cors)
}