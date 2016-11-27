byGeneTester <- function (my.fit, r.crit=.8, w.crit=.6, multiples) {
  # my.fit must be a data.frame and have column $egid

  nc <- ncol(my.fit)
  my.fit[,1:2] <- apply(my.fit[,1:2],2,as.character)

  tab <- table(my.fit$egid)
  singles <- names(tab)[which(tab==1)]
  new.ids <- my.fit[match(singles[1:10],my.fit$egid),1:2]
cat("Singles done.\n")  

#  multiples <- names(tab)[which(tab>2)]
  for (m in multiples) {
    old.rows <- get.rows.as.cols(m,my.fit)
print(old.rows)
    ids <- NULL
    mat <- getPairwiseMatrix(old.rows)
print(mat)
    w <- mean(mat)
cat(w,"\n")
    if (w < w.crit) {
### AS LONG AS R.CRIT >=W.CRIT, A CLIQUE SHOULD AUTOMATICALLY HAVE W > W.CRIT, RIGHT???
      cliqs <- getSubGraphs(mat,r.crit,m)
print(cliqs)
      for (i in 1:length(cliqs)) {
        cl <- unlist(cliqs[i])
        ids <- rbind(ids, cbind(affy.id=cl,egid=paste(m,i,sep='.')))
      }
    } else {
       ids <- cbind(affy.id=colnames(old.rows),egid=m)
    }
    new.ids <- rbind(new.ids, ids)
  }
  new.ids
}