get.p.sig <- function(m,q=5) {
  # q <- ceiling(abs(log10(min(p[which(m<1)]))))
  m <- abs(m)
  p.sig <- .01    # for values 1 < m < 1.5
  if (m>1.5)      { p.sig <- .01 + .02*min(m-1.5,2)
  } else if (m<1) { p.sig <- 10^(-2 + q*(m-1))  }
  p.sig
}

