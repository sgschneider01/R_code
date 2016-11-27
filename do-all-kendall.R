#source("~/Desktop/research/notes/R_code/do-all-kendall.R")
source("~/Desktop/research/notes/R_code/handyFunctions.R")
source("~/Desktop/research/notes/R_code/getCC.R")
source("~/Desktop/research/notes/R_code/getAnnEset.R")
source("~/Desktop/research/notes/R_code/analyzeGEO.R")
source("~/Desktop/research/notes/R_code/byGeneW.R")
source("~/Desktop/research/notes/R_code/consolidatePS.R")
source("~/Desktop/research/notes/R_code/getPsig.R")
source("~/Desktop/research/notes/R_code/new_decide_test.R")
source("~/Desktop/research/notes/R_code/addAnnotation.R")

do.all.w.kendall <- function ( exp.name, exp.platform, local=TRUE ) {
  require(affy)
setwd(paste("~/Desktop/research/notes/R_objects/",exp.name,sep=''))
  gf       <- paste(exp.name,"gf",sep='.')
  gf.file  <- paste(gf,"RData",sep='.')
  f3       <- paste(exp.name,"fit3",sep='.')
  f3.file  <- paste(f3,"RData",sep='.')
  f1       <- paste(exp.name,"fit1",sep='.')
  f1.file  <- paste(f1,"RData",sep='.')
  ids      <- paste(exp.name,"new.ids",sep='.')
  ids.file <- paste(ids,"RData",sep='.')

  if (!exists(ids)) {
    if (file.exists(ids.file)) {
      load(ids.file,.GlobalEnv)
      load(gf.file,.GlobalEnv) 
      load(f1.file,.GlobalEnv)
      load(f3.file,.GlobalEnv)
    } else {  
      if (!exists(gf)) { 
        if (file.exists(gf.file)) {   # *** should this be checking for f3.file?
          load(gf.file,.GlobalEnv) 
          load(f1.file,.GlobalEnv) # should only be needed when .gf is loaded
          load(f3.file,.GlobalEnv) # should only be needed when .gf is loaded
        } 
      }
      if (!exists(f3)) {
        my.eset <- analyzeGEO(exp.name,local=local)  # will create one/both as needed
      }
      
      my.eset <- getAnnEset(do.eval(gf),exp.plat) 
      new.ids <- byGeneW(my.eset)
cat("byGeneW done\n")
      new.ids <- subset(new.ids,!is.na(new.ids$affy.id))
#      new.ids <- new.ids[match(my.eset$affy.id,new.ids$affy.id),]  
      #reorder to match original order
      assign(ids, new.ids, pos=1)
      save(list=c(as.character(ids)), file=ids.file)
    }
  }

  new.ids <- do.eval(ids)
  fit3 <- do.eval(f3) 
  fit4 <- consolidate.ps (fit3, new.ids)
  fit4
}

#for (td in to.do) {
#   exp.name     <- td[[1]]
#   exp.platform <- td[[2]]
#   fit4     <- paste(exp.name,"fit4",sep='.')
#   assign(fit4,do.all.w.kendall(exp.name,exp.platform,TRUE))
#}

#  fit3$decision <- decide.test(fit3)  # for comparison purposes need to assign or return!
#  fit4$decision <- decide.test(fit4)
# come up with some pretty print function for this?
#    need to add symbol (and name) annotation too
