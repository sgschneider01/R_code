#source("~/Desktop/research/notes/R_code/analyzeGEOpool.R")
source("~/Desktop/research/notes/R_code/analyzeGEO.R")
source("~/Desktop/research/notes/R_code/addAnnot.R")
source("~/Desktop/research/notes/R_code/addCounts.R")
source("~/Desktop/research/notes/R_code/handyFunctions.R")

analyzeGEOpool2 <- function (ctr.list, gse.pool, aff.chip, pval=.05, mval=1, adjm="none") {

#Takes:    ctr.list = a string indicating which contrast list to use eg. M66
#          gse.pool = a string indicating which gse name to use eg. bcpool
#          aff.chip = name of the aff.chipfy chip (for annotation) eg mouse4302
#          P-value, log fold change, and adjustment method to use in
#             decideTests() (default: p<.05, lfc=1, "none")
#Creates:  gse.pool object (created by analyzeGEO())
#Returns:  fat = matrix of probes, direction in each contrast, plus counts and annotations

require(GEOquery)
setwd("~/Desktop/research/notes/R_objects")

# Step 1:  Get the gsm-to-condition list
   g2c <- paste(gse.pool,".g2c",sep='')
   if (!exists(g2c)) {
      gsm2cond <- read.table(file=paste(gse.pool,"-g2c.txt",sep=''))
   } else {
      gsm2cond <- eval(parse(text=g2c))
   }
cat("analyzeGEOpool() step 1 completed, gsm2cond created.\n")

# Step 2:  Get the contrasts list
   if (!exists(paste(ctr.list,".ctr",sep=''))) {
      ctr.list <- scan(paste(ctr.list,"-contrasts.txt",sep=''),what="character")
   } else {
      ctr.list <- eval(parse(text=paste(ctr.list,".ctr",sep='')))
   }
cat("analyzeGEOpool() step 2 completed, ctr.list created.\n")

# Step 3:  Get the decideTests TestResults matrix.
   fit4 <- analyzeGEO(gse.pool,gsm2cond,ctr.list,pval,mval,adjm)

   return(fit4)
}