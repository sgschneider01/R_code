getRcrit <-
function(a,n)  {
# a is alpha, the p value cutoff
# n is the number of observations ie samples not judges (n-2 degrees of freedom)
   t <- 0-qt(a,n-2)
   sqrt(t^2/(t^2+n-2))
}

