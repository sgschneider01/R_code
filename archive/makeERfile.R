makeERfile <- function (x) {
  # x is a list of length one, with a name and a sublist - separate them:
  n <- names(x)[1]
  x <- x[[1]]

  f <- paste(n,".fit",sep='')
  if (!exists(f)) {
      fit <- summarizeChips(n,x)
      fit <- as.data.frame.MArrayLM(fit)
      fit <- fit[,grep("coeff",colnames(fit))]
      #   get just the columns with the expression levels ("coefficients")
      assign (f, fit, pos=1) 
  }

  e <- paste(n,".er",sep='')
  eo <- getERlevels(f,esr.hgu)
  assign (e, eo, pos=1)

  f2 <- paste("~/Desktop/research/geo-analysis/er-levels/",
              paste(n,"_er.txt",sep=''), sep='')
cat("      Writing file:",f2,"\n")
  write.table(eo,file=f2,sep='\t',quote=FALSE)
  
}