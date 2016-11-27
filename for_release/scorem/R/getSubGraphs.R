getSubGraphs <-
function (object,alpha,nc,w.crit) {

   r.crit <- getRcrit(alpha,nc) 
   # w.crit stays the same, r.crit increases with recursion

   cc <- connComp(as(new("graphAM",object>r.crit),"graphNEL"))  
   sg<-NULL
   for (x in cc) {
      mx <- object[x,x]
      w  <- mean(mx)
      n  <- length(x)

      if (n==2) {
        if (w < w.crit) { x <- as.list(x) } 
        # otherwise, leave cc intact
      } else if (n==3) { 
        if (w < w.crit) { 
          rs <- c(mx[1,2],mx[1,3],mx[2,3])
          if (max(rs)>r.crit) {             # consolidate two
            pairs  <- list(c(1,2),c(1,3),c(2,3))
            tog <- pairs[[which.max(rs)]]   # pair to consolidate
            sep <- setdiff(1:3,tog)         # single to leave out
            x <- list(x[tog],x[sep])
          } else {                          # don't consolidate any
            x <- as.list(x) 
          }
        } 
      } else { 
        if (w < w.crit) { x <- getSubGraphs(mx,alpha,nc-2,w.crit) } 
        # otherwise, leave cc intact
      }

     if (!is.list(x))  { x <- list(x) }  # NOTE: different than as.list(x)!!
     sg <- c(sg,x)
   }
   sg
}

