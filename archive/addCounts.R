addCounts <- function (fit4) {

  nf4<-ncol(fit4)

  if (nf4>1) {
    fit4<-cbind(fit4,apply(fit4[,1:nf4],1, function(x) { sum(x>0) } ))
    fit4<-cbind(fit4,apply(fit4[,1:nf4],1, function(x) { sum(x<0) } ))
    fit4<-cbind(fit4,apply(fit4[,1:nf4],1, function(x) { sum(abs(x)) } ))
    fit4<-cbind(fit4,apply(fit4[,1:nf4],1, function(x) { sum(x) } ))
    fit4<-cbind(fit4,apply(fit4[,1:nf4],1, function(x) { mean(abs(x)) } ))
    colnames(fit4)[(nf4+1):(nf4+5)] <- 
       c("count.up","count.down","count.all","sum","percent.DE")
  } else {
    colnames(fit4)[1]<-"count.all"
  }

  fit4 <- cbind(affy.id=row.names(fit4),fit4)
  rownames(fit4) <- 1:nrow(fit4)
  fit4<-fit4[which(fit4$count.all>0),]
  fit4<-fit4[order(fit4$count.all,decreasing=TRUE),]
cat("Counts are done.\n")
  fit4
}
