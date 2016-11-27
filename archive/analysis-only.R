source("~/Desktop/research/notes/R_code/makeGSE.R")
source("~/Desktop/research/notes/R_code/makeDesign.R")
source("~/Desktop/research/notes/R_code/analyzeChips.R")
require(limma)
require(genefilter)

gse <- "ROonly"
ctr.list <- "R3" 
gf <- TRUE
               

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

#######  Step 2a:  Get the GSM files from GEO and convert to ExpressionSet #####
   if (!exists(gse))  { 
      if (grepl("KK",gse) | grepl("RO",gse))  { 
        assign (gse, analyzeChips(gse,"gcrma"), pos=1)
      } else { 
        assign (gse, makeGSE(rownames(gsm2cond)), pos=1) 
      } 
   }
   eset <- eval(parse(text=gse))


#######  Step 2b:  Filter to remove least variable probe sets             #####
   eset.gf <- eset
   if (gf) {
      f1 <- pOverA(0.25, log2(100))
      f2 <- iqrOverA(0.5)  #in handyFunctions.R
      ff <- filterfun(f1,f2)
      rows.gf <- genefilter(exprs(eset.gf),ff)
      exprs(eset.gf) <- exprs(eset.gf)[rows.gf,]
   }

#######  Step 3:  Fit linear model for each gene (exprSet => MArrayLM)     #####
   design.matrix <- makeDesign(eset.gf,gsm2cond)
   fit1 <- lmFit(eset.gf,design.matrix)
   
#######  Step 4:  Create pairwise comparisons (M values)                   #####
   contrast.matrix <- makeContrasts(contrasts=ctr.list,levels=colnames(design.matrix))
   fit2 <- contrasts.fit(fit1,contrast.matrix)

#######  Step 5:  Get test statistics for pairwise comparisons (p values)  #####
   fit3 <- eBayes(fit2)


