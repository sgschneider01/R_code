getDT <- function (exp.name,exp.platform,dt="old",p=.01,m=1,am="BH") {
  fit3     <- do.eval(paste(exp.name,"fit3",sep='.'))
  fit4     <- do.eval(paste(exp.name,"fit4",sep='.'))
  new.ids  <- do.eval(paste(exp.name,"new.ids",sep='.'))
  new.ids  <- addAnnotation(new.ids, exp.platform, "symbol")
   
  switch (dt, 
    old = {
      dt1<-my.decideTests(fit3,p.value=p,lfc=m,adjust.method=am)
      dt1<-subset(dt1[,],apply(dt1,1,function(x)any(x!=0)))  
      dt1<-merge(new.ids,dt1,by.x="affy.id",by.y=0)
      assign(paste(c(exp.name,"dt.old",m,p,am),sep='',collapse='.'), dt1, pos=1)
    },
    new = {
      dt4<-new.decide.test(fit4)
      dt4<-subset(dt4[,],apply(dt4,1,function(x)any(x!=0)))
      dt4<-merge(unique(new.ids[,2:3]),dt4,by.x="new.eg.id",by.y=0) 
      assign(paste(exp.name,"dt4",sep='.'), dt4, pos=1)
    }
  )
}