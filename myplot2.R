my.plot2 <- function (g,f,sp) {
  mat <- get.rows.as.cols(g,f)
  nc <- ncol(mat)
  pm<-getPairwiseMatrix(mat)
  w<-round(mean(pm,na.rm=T),3)

  conds<-as.factor(gsub("[.].*","",rownames(mat)))
  groups<-lapply(levels(conds),function(x)which(conds==x))

# get the info for the labels
  ann<-paste("org",sp,"egSYMBOL",sep='.')
  t<-get(as.character(g),do.eval(ann))
  id <-paste(t," (",g,")",sep='')
  lbls<-c(colnames(mat),"consolidated")

# set up the axes
  pretty <- c(rainbow(nc),"black")
  ymin<-min(min(mat),2)
  ymax<-max(max(mat),15)
  plot(0,0,main=id,xlim=c(1,nrow(mat)),ylim=c(ymin,ymax),xaxt="n", xlab="Condition", ylab="log2(expression)")
  axis(1,at=1:nrow(mat),conds,las=2,cex.axis=.75)
  
# print the labels
  legend("topright",lbls,lty=1,bty="n",col=pretty,cex=.5)
  legend("topleft",paste("w=",w,sep=''),bty="n")

# plot the points  
  for (i in 1:nc) {
     for (j in groups) {
        lines(x=j,y=mat[j,i],col=pretty[i],lwd=2)
     }
  }
  tap<-tapply(rowMeans(mat),conds,function(x)rep(mean(x),length(x)))
  tap<-unlist(tap,use.names=F)
  for (j in groups) {
     lines(x=j,y=tap[j],col="black",lwd=2)
  }
   
}

