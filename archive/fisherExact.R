fisher.exact <- function(mat) {
   if (!is.matrix(mat) || ncol(mat)!=2 || nrow(mat)!=2) { 
      stop("mat must be a matrix representing a 2x2 contingency table.\n") 
   }
   
# NOTE: factorial() maxes out at factorial(170)

   a<-factorial(mat[1,1])
   b<-factorial(mat[1,2])
   c<-factorial(mat[2,1])
   d<-factorial(mat[2,2])
   n<-factorial(sum(mat))
   a.b<-factorial(sum(mat[1,]))
   c.d<-factorial(sum(mat[2,]))
   a.c<-factorial(sum(mat[,1]))
   b.d<-factorial(sum(mat[,2]))
   p.value <- (a.b*c.d*a.c*b.d)/(n*a*b*c*d)
   cat("The one-sided p-value for the contigency table is",p.value,"\n")
   return(p.value)
}

hyperg.test <- function (mat) {
   if (!is.matrix(mat) || ncol(mat)!=2 || nrow(mat)!=2) { 
      stop("mat must be a matrix representing a 2x2 contingency table.\n") 
   }

#choose(n,k) maxes out at n=40000,k=102 or 20000,116 or 10000,134
# above these numbers returns infinity (Inf)

   a<-mat[1,1]        # o or k
   a.b<-sum(mat[1,])  # e or m
   c<-mat[2,1]
   c.d<-sum(mat[2,])  # (p-e) or (N-m)
   n<-sum(mat)        # p or N
   a.c<-sum(mat[,1])  # s or n
   p.value <- (choose(a.b,a) * choose(c.d,c)) / choose(n,a.c)
   cat("The one-sided p-value for the contigency table is",p.value,"\n")
   return(p.value)
}

proptest <- function(mat) {
   o<-mat[1,1]      # OBSERVED
   e<-sum(mat[1,])  # EXPECTED
   s<-sum(mat[,1])  # SIZE OF THE SAMPLE
   p<-sum(mat)      # SIZE OF THE POPULATION
   prop.test(c(o,e),c(s,p))
}

binom <- function(mat) {
   o<-mat[1,1]
   e<-sum(mat[1,]) 
   n<-sum(mat[,1])
   N<-sum(mat)
   binom.test(o,n,p=(e/N))
}
