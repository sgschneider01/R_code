doKendall.list <- function (tab, my.fit, r.crit=.8, w.crit=.7) {
  source("~/Desktop/research/notes/R_code/getR.R")
  source("~/Desktop/research/notes/R_code/getCliques.R")
  source("~/Desktop/research/notes/R_code/getW.R")
new.data <- NULL

  nc<-ncol(my.fit)
  doubles <- names(tab)[which(tab==2)]
  for (d in doubles) {
     old.rows <- my.fit[which(my.fit$egid==d),3:nc]  
     r <- getR(old.rows)
     if (r > r.crit) {
       new.data<-rbind(new.data,c(d,2,r,1))
     } else {
       new.data<-rbind(new.data,c(d,2,r,0)) 
     }
  }

  multiples <- names(tab)[which(tab>2)]
  for (m in multiples) {
     old.rows <- get.rows.as.cols(m,my.fit)
     w <- getW(old.rows)
     if (w > w.crit) {
         new.data<-rbind(new.data,c(m,ncol(old.rows),w,1))
     } else {
        pm <- getPairwiseMatrix(old.rows)
        cliqs <- getCliques(pm,r.crit)
        new.data<-rbind(new.data,c(m,ncol(old.rows),w,0))
        cat(m,"\n")
        print(cliqs)
    }
  }
  new.data
}


#        for each cliq 
#          get R or W or single value
#        consolidate each cliq
#        add cliqs to list of bad.w
