my.plot <- function (g,f=NULL,sp="Mm",ss=NULL) {

  eset.rows <- get.rows.as.cols(g,f)
  pretty <- rainbow(ncol(x))
  if(!is.null(ss)) x <- x[,ss]
  if(!is.null(ss)) pretty <- pretty[ss]

  pm<-cor(eset.rows,method="spearman")
  w<-round(mean(pm,na.rm=T),3)

  ann<-paste("org",sp,"egSYMBOL",sep='.')
  t<-get(as.character(g),do.eval(ann))
  id <-paste(t," (",g,")",sep='')

  matplot(x,main=id,type="l",col=pretty, lty=1,lwd=2,xlab="Sample", ylab="log2(expression)",xlim=c(1,nrow(x)),ylim=c(1,15))
  legend("topright",colnames(x),lty=1,bty="n",col=pretty,cex=.5)
  legend("topleft",paste("w=",w,sep=''),bty="n")
}

my.plot.w.mu <- function (g,f=NULL) {
  if (is.matrix(g) || is.data.frame(g))  { 
     x<-g
  } else {
     x <- get.rows.as.cols(g,f)
  }
  pretty <- c(rainbow(ncol(x)),"black")
  x <- cbind(x,rowMeans(x))

  t<-get(as.character(g),org.Mm.egSYMBOL)
  id <-paste(t," (",g,")",sep='')
  matplot(x,main=id,type="l",col=pretty, lty=1,lwd=2)
  legend("topright",colnames(x),lty=1,bty="n",col=pretty,cex=.5)
}

my.plot.w.avg <- function (g,f) {
  mat <- get.rows.as.cols(g,f)
  nc <- ncol(mat)

  pm<-getPairwiseMatrix(mat)
  w<-round(mean(pm),3)
  t<-get(as.character(g),org.Mm.egSYMBOL)
  id <-paste(t," (",g,")",sep='')

  pretty <- rainbow(nc)
  matplot(mat,main=id,sub=paste("w=",w),type="p",col=pretty, pch=20)
  legend("topright",colnames(mat),lty=1,bty="n",col=pretty,cex=.5)

conds <- colnames(my.fit)[3:nc]

  row.by.cond<-apply(mat, 2, function(x){tapply(x,conds,mean)})
  for(i in 1:ncol(row.by.cond)) {
    for (j in 1:length(groups)) {
      g <- groups[[j]]
      lines(x=g,y=rep(row.by.cond[j,i],length(g)),col=pretty[i])
    }
  }
}

my.plot.w.avgse <- function (mat) {
  pretty <- rainbow(ncol(mat))
  matplot(mat,type="p",col="white")
  legend("topright",colnames(mat),lty=1,bty="n",col=pretty,cex=.5)

  mean.by.cond<-apply(mat, 2, function(x){tapply(x,conds,mean)})
  se.by.cond<-apply(mat, 2, function(x){ 
                   tapply(x,conds, function(y) {sd(y)/sqrt(nrow(mat))})  })
  for(i in 1:ncol(mean.by.cond)) {
    for (j in 1:length(groups)) {
      g <- groups[[j]]
      gg <- groups2[[j]]
      ggg <- groups3[j]
      lines(x=g,y=rep(mean.by.cond[j,i],length(g)),col=pretty[i])
      lines(x=gg,y=rep(mean.by.cond[j,i]-se.by.cond[j,i],length(g)), col=pretty[i])
      lines(x=gg,y=rep(mean.by.cond[j,i]+se.by.cond[j,i],length(g)), col=pretty[i])
      lines(x=c(ggg,ggg),y=c(mean.by.cond[j,i]+se.by.cond[j,i], mean.by.cond[j,i]-se.by.cond[j,i]))
    }
  }
}

