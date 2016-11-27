new.decide.test <- function (my.fit,adj.meth=NULL) {
# returns a matrix of the m-values with signif p (the rest become 0)
  m <- my.fit$coefficients
  m.a <- abs(m)
  p <- my.fit$p.value

  if (!is.null(adj.meth)) p<-p.adjust(p,adj.meth)

  m.a.2 <- apply(m.a,c(1,2),function(x)min(x-1.5,2))
  p.sig <- ifelse (m.a>1.5, .01 + .02*m.a.2, 
                   ifelse (m.a<1, 10^(-7 + 5*m.a), .01))

  dt <- cbind(m*(p<=p.sig), p*(p<=p.sig))
  colnames(dt) <- c(paste(colnames(m),"m",sep='.'), paste(colnames(p),"p",sep='.'))  
  dt <- subset(dt[,],apply(dt,1,function(x)any(x!=0)))
  dt
}

# Cannot apply min to m.a as a matrix, will return one value
#  p.sig <- ifelse (m.a>1.5, .01 + .02*min(m.a-1.5,2), 

