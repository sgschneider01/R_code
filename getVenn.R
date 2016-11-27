getVenn <- function (x,y) {
# a pretty print function to show size of two sets and overlap/set diff

x.u<-unique(x)
y.u<-unique(y)
   mat<-matrix(c(length(x),length(y), 
                 length(x.u),length(y.u), 
                 length(intersect(x,y)),length(intersect(x,y)),
                 length(setdiff(x,y)),length(setdiff(y,x))),  
               ncol=2,byrow=T)
 
   colnames(mat) <- c("X","Y")
   rownames(mat) <- c("total size","non-duplicated","overlap","unique elements")
   print (mat)

   if (length(x.u)>length(y.u) & length(intersect(x,y))>.8*length(y.u)) {
     cat("X dominates Y\n") 
   } else if (length(y.u)>length(x.u) & length(intersect(x,y))>.8*length(x.u)) {
     cat("Y dominates X\n") 
   } else {
     cat("No dominant list \n")
   }
  
   mat
}

