# finds ones that are identical in sign across the contrasts

find.concord <- function(dt) { 

  egs <- neweg2old(rownames(dt))
  dt <- subset(dt,egs %in% get.duped(egs))
  if (nrow(dt)==0) return ()

  egs <- neweg2old(rownames(dt))  # reset after subsetting
  x <- tapply(rownames(dt),egs,
              function(y) subset(dt,rownames(dt) %in% y),
              simplify=F)
  # creates a list of arrays, one for each duplicated egid

  u <- sapply(x, function(y) {
                    any(duplicated(sign(y)))  # compares rows
             } )
  # tells us if any are concordant, not which (returns all)

#  concs <- subset(names(u),u)
#  return(concs)
   return(subset(x,u))
}

