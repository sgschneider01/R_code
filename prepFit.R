prepFit <- function (exp.name, exp.platform) {
  gf<-paste(exp.name,"gf",sep='.')
  fn<-paste(exp.name,"/",gf,".RData",sep='')
  if (!exists(gf)) load(fn,envir=.GlobalEnv)
  gsm2cond<-read.table(paste(exp.name,"/",exp.name,"-g2c.txt",sep=''))
  chips<-rownames(gsm2cond)[order(gsm2cond$Condition)]
  conds<-gsm2cond[order(gsm2cond$Condition),]  #drops the rownames

  my.fit<-exprs(do.eval(paste(exp.name,"gf",sep='.')))
  my.fit<-my.fit[,match(chips,colnames(my.fit))] 
  # just in case the g2c file had the names in a different order than the eset
  colnames(my.fit) <- conds
  my.fit<-addAnnotation(my.fit,exp.platform,"egid")
  my.fit
}


# preps for plotting or getPairwiseMatrix, getW, etc