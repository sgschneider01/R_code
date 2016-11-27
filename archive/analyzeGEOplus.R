#source("~/Desktop/research/notes/R_code/analyzeGEOplus.R")
source("~/Desktop/research/notes/R_code/makeGSE.R")
source("~/Desktop/research/notes/R_code/makeDesign.R")
source("~/Desktop/research/notes/R_code/analyzeChips.R")
source("~/Desktop/research/notes/R_code/perGene.R")

analyzeGEOplus <- function ( ctr.list, gse , gf=TRUE, anno, adj) {

#Takes:    1- ctr.list = a string indicating which contrast list to use eg. M66
#          2- gse = a string indicating which gse name to use eg. bcpool
#          3- gf = a logical value indicating whether or not to use genefilter
#Creates:  The GSE object, if not exists, from makeGSE()
#Returns:  fit3, the MArrayLM object with contrast p-values from eBayes()


#### Revision History ####
# 2009 August 7 - moved get g2c and contrs back here from analyzeGEOpool
# 2009 August 5 - added genefilter (step 2) with IQR > .5 and 25% of all values > log2(100)
# 2009 July  29 - moved decideTests() step back to analyzeGEOpool

require(multtest)
require(limma)
require(genefilter)

#######  Step 1:  Get the gsm-to-condition and contrasts list from files
   setwd("~/Desktop/research/notes/R_objects")
   g2c <- paste(gse,".g2c",sep='')
   if (exists(g2c)) {
      gsm2cond <- eval(parse(text=g2c))
   } else {
      gsm2cond <- read.table(file=paste(gse,"-g2c.txt",sep=''))
   }

   cl <- paste(ctr.list,".ctr",sep='')
   if (exists(cl)) {
      ctr.list <- eval(parse(text=cl))
   } else {
      ctr.list <- scan(paste(ctr.list,"-contrasts.txt",sep=''),what="character")
   }
cat("Step 1 completed.\n")

#######  Step 2a:  Get the GSM files from GEO and convert to ExpressionSet  #####
   if (!exists(gse))  { 
      if (grepl("KK",gse) | grepl("RO",gse))  {
         assign (gse, analyzeChips(gse,"gcrma"), pos=1) 
      } else {
         assign (gse, makeGSE(rownames(gsm2cond)), pos=1) 
      } 
   }
   eset <- eval(parse(text=gse))
cat("Step 2a completed, expressionSet made.\n")

#######  Step 2b:  Filter to remove least variable probe sets               #####
   if (gf) {
      f1 <- pOverA(0.25, log2(100))
      f2 <- iqrOverA(0.5)  #in handyFunctions.R
      ff <- filterfun(f1,f2)
      gf <- genefilter(exprs(eset),ff)
      exprs(eset) <- exprs(eset)[gf,]
cat("Step 2b completed, genefilter done.\n")
   }

#######  Step 3:  Fit linear model for each gene (exprSet => MArrayLM)     #####
   design.matrix <- makeDesign(eset,gsm2cond)
   fit1a <- lmFit(eset,design.matrix)

   anno <- addAnnot(NULL,anno)
   fit1b <- perGene(eset,anno,design=gsm2cond[sampleNames(eset),1],adj.meth="BH",alpha=.01)
cat("Step 3 completed, lmFit done.\n")
   
#######  Step 4:  Create pairwise comparisons (M values)                   #####
#   contrast.matrix <- makeContrasts(contrasts=ctr.list,levels=colnames(design.matrix))
#   fit2 <- contrasts.fit(fit1,contrast.matrix)
cat("Step 4 completed, contrasts.fit done.\n")

#######  Step 5:  Get test statistics for pairwise comparisons (p values)  #####
#   fit3 <- eBayes(fit2)
cat("Step 5 completed, eBayes done.\n")
   return(list(fit1a,fit1b))
}
