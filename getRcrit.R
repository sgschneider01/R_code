get.r.crit <- function(a,n)  {
# a is alpha, the p value cutoff
# n is the number of observations ie samples not judges (n-2 degrees of freedom)
   t <- 0-qt(a,n-2)
   sqrt(t^2/(t^2+n-2))
}

get.w.crit <- function (a,m,n) {
# Friedman's chi square for W
  x2   <- qchisq(a,n-1,lower.tail=F)
  w.cr <- x2/(m*(n-1))
  w.cr
}
