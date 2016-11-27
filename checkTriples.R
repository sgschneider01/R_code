checkTriples <- function (pm, r.crit, w.crit) {
#Note: getPairwiseMatrix should give the simple mean, not Kendall's, 
#  since for n<4 it puts in NAs  

  if (nrow(pm)!=3)
    stop("Must be a 3x3 matrix.")
  rs <- unique(as.vector(pm))
  rs <- subset(rs,!is.na(rs))
  mu <- mean(rs) 
  n.good <- sum(rs>r.crit)
  pairs  <- list(c(1,2),c(1,3),c(2,3))

  if ( n.good==3 || (n.good==2 & mu>r.crit) ) {
    # all good || one bad w/ good W -> consolidate all 3
    return(list(c(1,2,3)))
  } else if ( n.good==2 & mu<r.crit ) {
    # consolidate the best two?
    y <- pairs[[which.max(rs)]]
    n <- setdiff(1:3,y)
    return (list(y,n))
  } else if ( n.good==1 ) {
    # one good pair -> consolidate those two
    y <- pairs[[which(rs>r.crit)]]
    n <- setdiff(1:3,y)
    return (list(y,n))
  } else {  
    # reannotate all 3
    return (list(1,2,3))
  }
}

