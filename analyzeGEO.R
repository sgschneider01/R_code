#source("~/Desktop/research/notes/R_code/analyzeGEO.R")
source("~/Desktop/research/notes/R_code/analyzeChips.R")
source("~/Desktop/research/notes/R_code/filterGenes.R")
source("~/Desktop/research/notes/R_code/getCC.R")
source("~/Desktop/research/notes/R_code/makeDesign.R")
source("~/Desktop/research/notes/R_code/makeGSE.R")

analyzeGEO <- function ( exp.name, gf=TRUE, raw=FALSE) {

#Takes:    1- exp.name = a string indicating which contrast list to use eg. M66
#          2- gf = a logical value indicating whether or not to use genefilter
#Creates:  fit1,fit2, and fit3
#Returns:  eset.gf, the filtered, normalized unprocessed data


#### Revision History ####
# 2009 August 7 - moved get g2c and contrs back here from analyzeGEOpool
# 2009 August 5 - added genefilter (step 2) with IQR > .5 and 25% of all values > log2(100)
# 2009 July  29 - moved decideTests() step back to analyzeGEOpool
# 2010 June  18 - added save/load steps


require(limma)
require(genefilter)
  
#####  Step 1:  Get the gsm-to-condition and contrasts list from files
   gsm2cond <- getConditions(exp.name)
   cntrsts  <- getContrasts(exp.name)
cat("Step 1 completed.\n")

#####  Step 2a:  Get the GSM files from GEO and convert to ExpressionSet  #####
#####  Step 2b:  Filter to remove least variable probe sets               #####
   gf.name   <- paste(exp.name,"gf",sep=".")
   eset.name <- paste(exp.name,"eset",sep='.')
   eset.file <- paste(eset.name,"RData",sep='.')
   if (!exists(gf.name)) { 
     if (!exists(eset.name)) { 
       if (file.exists(eset.file)) { 
         load(eset.file,.GlobalEnv)
       } else {
         if (raw) { 
           eset <- analyzeChips(exp.name,"gcrma") 
         } else  { 
           eset <- makeGSE(rownames(gsm2cond)) 
         }
         assign(eset.name, eset, pos=1)
         save(list=eset.name, file=eset.file)
       }
     }
     eset <- do.eval(eset.name)
     gf <- filterGenes(eset,gsm2cond)
     assign(gf.name, gf, pos=1)
     save(list=gf.name, file=paste(gf.name,"RData",sep='.'))
   } else { 
     gf <- do.eval(gf.name)
   }
cat("Step 2 completed.\n")


#######  Step 3:  Fit linear model for each gene (exprSet => MArrayLM)     #####
   design.matrix <- makeDesign(gf,gsm2cond)
   fit1 <- lmFit(gf,design.matrix)
   f1<-paste(exp.name,"fit1",sep='.')
   assign(f1,fit1,pos=1)
   save(list=c(as.character(f1)), file=paste(exp.name,"fit1","RData",sep='.')) 
cat("Step 3 completed, lmFit done.\n")


#######  Step 4:  Create pairwise comparisons (M values)                   #####
   contrast.matrix <- makeContrasts(contrasts=cntrsts,levels=colnames(design.matrix))
   fit2 <- contrasts.fit(fit1,contrast.matrix)
cat("Step 4 completed, contrasts.fit done.\n")


#######  Step 5:  Get test statistics for pairwise comparisons (p values)  #####
   fit3 <- eBayes(fit2)
cat("Step 5 completed, ",exp.name,"done.\n")

   f3 <- paste(exp.name,"fit3",sep='.')
   assign(f3,fit3,pos=1)
   save(list=c(as.character(f3)), file=paste(exp.name,"fit3","RData",sep='.')) 

   return(1)
}
