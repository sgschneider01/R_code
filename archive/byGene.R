byGene <- function (my.fit, r.crit) {
source("getCliques.R")
source("getExons.R")
source("getPairwiseMatrix.R")
source("getPairwiseCorr.R")
cat(Sys.time())

  nc <- ncol(my.fit)

# Assumes NA'S DROPPED ENTIRELY
  #get list of duplicated EGIDs
  tab<-table(my.fit$egid)

  #just copy singles
  singles <- names(tab)[which(tab==1)]
  new.data <- NULL
  new.data <- rbind(new.data, my.fit[match(singles,my.fit$egid),] )
  
cat("done with singles\n")  
  
  #deal with doubles
  doubles <- names(tab)[which(tab==2)]
  for (d in doubles[1:50]) {
     old.rows <- my.fit[which(my.fit$egid==d),3:nc]  
       #old.rows[,1]  is considered a data frame, won't work in cor.test
     x<-as.vector(old.rows[1,],mode="numeric")
     y<-as.vector(old.rows[2,],mode="numeric")
if (sd(x)==0 && sd(y)==0) next  
if (sd(x)==0)  {
   new.row  <- c(do.eval(d),y,FALSE)
   new.data <- rbind(new.data,new.row)
   next
}
if (sd(y)==0)  {
   new.row  <- c(do.eval(d),x,FALSE)
   new.data <- rbind(new.data,new.row)
   next
}
     r<-cor.test(x,y)$estimate
     if (!is.na(r)) {
       if(r>r.crit) {  
         new.row  <- c(do.eval(d),apply(old.rows,2,mean),TRUE)
         new.data <- rbind(new.data, new.row )
       } else {
         new.row1  <- c(do.eval(d),x,FALSE)
         new.row2  <- c(do.eval(d),y,FALSE)
         new.data <- rbind(new.data,new.row1,new.row2)
       }
     } # no else - if (is.na(r)) - just drop it?  will r ever be na if sd>0?
  }   
cat("done with doubles\n")
}

#remove outliers from 3+
  multiples <- names(tab)[which(tab>2)]
  
  for (m in mults) {
    old.rows <- get.rows.as.cols(m,my.fit)
    mat <- getPairwiseMatrix(old.rows,method="spearman")
#    if (all(mat>r.crit)) {
    if (mean(mat) >= W.crit) {
       new.row  <- c(do.eval(m),apply(old.rows,2,mean),TRUE)
       new.data <- rbind(new.data, new.row )
    } else {
       cliqs <- getCliques(mat)
       cliqs <- lapply(cliqs,function(x){sub("n","",x)}) # get rid of "n" added by graphAM
       cliqs <- lapply(cliqs,as.numeric)

       if (any(duplicated(unlist(cliqs)))) {
         cat("overlaps")
       }

      for (cl in cliqs) {
         # what to do with a clique that is very low
         # what to do with overlapping cliques ie clique missing one edge
         p<-paste("FALSE",paste(cl,collapse=''),sep=":")
         if(length(cl)>1) {
           new.row  <- c(do.eval(m),apply(old.rows[,cl],2,mean),p)
         } else {
           new.row  <- c(do.eval(m),old.rows[,cl],p)
         }
         new.data <- rbind(new.data, new.row )
      }
    }
  }

cat(Sys.time())

return (new.data)

}