#source("~/Desktop/research/notes/R_code/analyzeGEOpool.R")
source("~/Desktop/research/notes/R_code/addAnnot.R")
source("~/Desktop/research/notes/R_code/addCounts.R")

analyzeGEOpool <- function (fit3, aff.chip, pval=.05, mval=1, adjm="none") {

#Takes:    fit3 = 
#          aff.chip = name of the affy chip (for annotation) eg mouse4302
#          pval, mval, adjm = p-value, log fold change, and adjustment method to
#             use in decideTests() (defaults: p<.05, lfc=0, "BH")
#Returns:  fat = matrix of probes, direction in each contrast, plus counts and annotations

#### Revision History ####
# 2009 August 25 - changed 10% requirement for DEG to apply only to larger pools (>15 contrasts, does not apply to KK(3) or RO(11 or 15))


require(GEOquery)
require(limma)

# Step 1:  Get list of DEGs from decideTests (same as topTable/decideCounts)
   fit4 <- decideTests(fit3,adjust.method=adjm,p.value=pval,lfc=mval)
   fit4 <- as.data.frame(unclass(fit4)) 
   # converts TestResults matrix into a regular matrix then a data.frame
cat("analyzeGEOpool() step 1 completed, fit4 created.\n")

# Step 2:  Add counts of number of contrasts in which each probeset is DE.
   nc <- ncol(fit4)
   fit4 <- addCounts(fit4)
   if (nc > 15)  fit4 <- fit4[which(fit4$count.all>(nc/10)),]
#     (select only those DE in >10% contrasts, for large pools)
cat("analyzeGEOpool() step 2 completed, fit4.top created.\n")

# Step 3:  Add annotations to matrix.
   if (!is.na(aff.chip))  { fat <- addAnnot(fit4,aff.chip)
   } else                 { fat<-fit4 }
cat("analyzeGEOpool() step 3 completed, fat created.\n")
#   write.table(fat,paste(ctr.list,"-top-ann.txt",sep=''),sep='\t',quote=FALSE)
   return(fat)
}