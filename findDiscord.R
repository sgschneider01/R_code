# finds the ones where any column (contrast) has two values in opposite directions (length unique sign > 1)
#  (function get.duped is in handyFunctions.R)

find.discord <- function(mat) { 
  egs <- neweg2old(rownames(mat))
  mat <- subset(mat, egs %in% get.duped(egs))
  if (nrow(mat)==0) return ()

  egs <- neweg2old(rownames(mat))  # reset after subsetting
  x <- tapply(rownames(mat),egs,
              function(y) subset(mat,rownames(mat) %in% y),
              simplify=F)
  # creates a list of arrays, one for each egid

  if (ncol(mat)>1) { 
     u <- subset(x,sapply(x,function(y) {
       any(apply(y,2,function(z) {
         length(unique(sign(subset(z,z!=0)))) > 1
       }))
     }))
  } else {
     # when there is only one contrast
     u <- subset(x,sapply(x,unction(y) {
        length(unique(sign(subset(y[,1],y[,1]!=0)))) > 1
     }))
  }
  return (u)
}
