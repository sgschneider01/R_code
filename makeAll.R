make.all <- function (gse.id) {
  system(paste("mkdir",gse.id))
  setwd(gse.id)
  gsm2cond <- makeGsm2Cond(gse.id)  # may need editing!!!
  gsm2cond <- getConditions(gse.id)
  # if gsm2cond from getConditions use rownames(gsm2cond) not gsm2cond[,1] below
 setwd("..")
}
  if (!is.null(gsm2cond)) {
    eset <- makeGSE(rownames(gsm2cond))  
    eset.name <- paste(gse.id,"eset",sep='.')
    eset.file <- paste(eset.name,"RData",sep='.')
    assign(eset.name, eset, pos=1)
    save(list=eset.name,file=eset.file)
  }
  setwd("..")

}