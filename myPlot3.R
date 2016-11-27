# plots points for each sample within the same condition at one x value 

my.plot3 <- function (g,f,sp,ss=NULL) {
  mat <- get.rows.as.cols(g,f)
  pretty <- c(rainbow(ncol(mat)),"black")
  if (!is.null(ss)) {
    mat<-mat[,ss]
    pretty <- pretty[ss]
  }
  pm  <- getPairwiseMatrix(mat)
  w   <- round(mean(pm,na.rm=T),3)

  conds  <- as.factor(gsub("[.].*","",rownames(mat)))
  groups <- lapply(levels(conds),function(x)which(conds==x))
  
# get the info for the labels
  ann<-paste("org",sp,"egSYMBOL",sep='.')
  t<-get(as.character(g),do.eval(ann))
  id <-paste(t," (",g,")",sep='')
  lbls<-c(colnames(mat),"consolidated")

# set up the axes
  plot(0,0,main=id,xlim=c(1,length(groups)),ylim=c(2,15), xaxt="n", xlab="Condition", ylab="log2(expression)")
  axis(1,at=1:length(groups),levels(conds),las=2,cex.axis=.75)

# print the labels
  legend("topright",lbls,lty=1,bty="n",col=pretty,cex=.5)
  legend("topleft",paste("w=",w,sep=''),bty="n")

# plot the points
  for (i in 1:ncol(mat)) {                   # for each probe set
     for (j in 1:length(groups)) {    # for each group (condition)
        k <- groups[[j]]
        points(x=rep(j,length(k)),y=mat[k,i],col=pretty[i])
        points(x=j,y=mean(mat[k,i]),col=pretty[i],pch="-",cex=2)
     }
  }
  
  tap<-tapply(rowMeans(mat),conds,mean)
  for (t in 1:length(tap)) {
     points(x=t,y=tap[t],col="black",pch="-",cex=2)
  }
}

