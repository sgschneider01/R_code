byGeneW <- function (my.eset,exp.plat,alpha=.01,drop.na=TRUE) {
  # exprs.eset must be a data.frame and have columns $affy.id and $egid
  exprs.eset <- exprs(my.eset)

  exprs.eset <- addAnnotation(exprs.eset, exp.plat, "egid")
  if (drop.na) exprs.eset <- subset(exprs.eset, exprs.eset$egid!="NA")
    
  nc <- ncol(exprs.eset)-2  # number of samples, not counting annotation cols
  st.t <- 0-qt(alpha,nc-2)
  r.crit <- sqrt(st.t^2/(st.t^2+nc-2))
  w.crit <- (1+r.crit)/2

  exprs.eset[,2] <- as.character(exprs.eset[,2]) # not factor
  tab <- table(exprs.eset$egid)

  singles <- subset(names(tab),tab==1)
  new.ids <- exprs.eset[1,1:2]

  doubles <- subset(names(tab),tab==2)
  for (d in doubles) {
    old.rows <- exprs.eset[which(exprs.eset$egid==d),] 
    r <- getR(old.rows[,3:nc])
    ids <- old.rows[,1:2]
    if (r > r.crit) {
      ids$egid <- paste(ids$egid[1],"1_2",2,sep='.')
    } else {
      ids$egid <- paste(ids$egid,1:2,2,sep='.')
    }
    new.ids <- rbind(new.ids, ids)
  }

  multiples <- subset(names(tab),tab>2) 
  for (m in multiples) {
    mat <- getPairwiseMatrix(m,exprs.eset)
    w   <- mean(mat,na.rm=TRUE)  # should have NAs for n=3
    ids <- data.frame(affy.id=colnames(mat),egid=m,stringsAsFactors=F)
    nr   <- nrow(mat)  # number of probe sets or judges
    if (w < w.crit) {
      sg <- getSubGraphs(mat,alpha,nc,w.crit)  # returns names not indices
      for (s in sg) {
        if (!is.numeric(s))  s <- match(s,ids$affy.id)
        sfx <- paste(s,collapse="_")
        ids$egid[s] <- paste(ids$egid[s[[1]]],sfx,nr,sep='.')
      }
    } else {
      ids$egid <- paste(ids$egid[1],paste(1:nr,collapse='_'),nr,sep='.')
    }
    new.ids <- rbind(new.ids, ids)
  }
  colnames(new.ids) <- c("affy.id","new.eg.id")
  new.ids[-1,]
}