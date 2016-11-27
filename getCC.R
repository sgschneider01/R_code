getConditions <- function (exp.name) {
   gsm2cond <- read.table(file=paste(exp.name,"-gsm2cond.txt",sep=''),sep='\t', header=T,row.names=1,as.is=T)
   gsm2cond
}

getContrasts <- function (exp.name) {
   cl <- paste(exp.name,".ctr",sep='')
   if (exists(cl)) {
      cntrsts <- eval(parse(text=cl))
   } else {
      cntrsts <- scan(paste(exp.name,"-contrasts.txt",sep=''),what="character")
   }
   cntrsts
}

