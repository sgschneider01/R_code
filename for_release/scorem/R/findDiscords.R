findDiscords <-
function(object) { 
  object <- subset(object,apply(object,1,function(x)any(x!=0)))
  egs <-   gsub("[.].*","",rownames(object))
  object <- subset(object, egs %in% subset(egs,duplicated(egs)))
  if (nrow(object)==0) return ()

  egs <-   gsub("[.].*","",rownames(object))  # reset after subsetting
  x <- tapply(rownames(object),egs,
              function(y) subset(object,rownames(object) %in% y),
              simplify=F)
  # creates a list of arrays, one for each egid

  if (ncol(object)>1) { 
    u <- sapply(x,function(y) {
      t(subset(t(y),apply(y,2,function(z) {
         length(unique(sign(subset(z,z!=0)))) > 1
      })))
    })
  } else {
     # when there is only one contrast
     u <- sapply(x,function(y) {
        length(unique(sign(subset(y[,1],y[,1]!=0)))) > 1
     })
  }
  u <- subset(u, sapply(u,function(v)ncol(v)>0))
  return (u)
}

