source("~/Desktop/research/notes/R_code/getRows.R")

getPairwiseMatrix <- function (x,f=NULL,method="spearman",w=TRUE,ss=NULL) {
  # df is a dataframe with k columns for each probe set and n rows for each chip
  if (!(is.data.frame(x) || is.matrix(x)))  {
     df <- get.rows.as.cols(x,f,ss=ss)
  }  else {
     df <- x
  }
  nc   <- ncol(df)
  cors <- matrix(0,nrow=nc,ncol=nc)
  
  for (i in 1:nc) {
     for (j in 1:nc) {
        if (i<j) {
          r <- cor(df[,i],df[,j],method=method)
          if (!is.na(r)) cors[i,j] <- r  # else leave cors[i,j]=0
        } else if (i==j) {
          cors[i,j] <- 1 
        } else if (i>j) {
          cors[i,j] <- cors[j,i]
        }
     }
  }
  colnames(cors) <- colnames(df)
  rownames(cors) <- colnames(df)
  cors
}

# r <- suppressWarnings(cor.test(df[,i],df[,j],method=method)$estimate)
