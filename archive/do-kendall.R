#source("~/Desktop/research/notes/R_code/do-all.R")
library(limma)
library(annotate)       # getGI()
library(Biostrings)     # matchPattern(), pairwiseAlignment(), etc.
library(XML)            # xmlParse(), etc.
library(RCurl)          # getURL()
#library(hgu133plus2.db) # auto calls library(org.Hs.eg.db)
library(mouse4302.db)   # auto calls library(org.Mm.eg.db)
library(mouse4302probe)
data(mouse4302probe)
setwd("~/Desktop/research/notes/R_code")
source("~/Desktop/research/notes/R_code/handyFunctions.R")
source("~/Desktop/research/notes/R_code/addEGID.R")
source("~/Desktop/research/notes/R_code/makeGSE.R")
source("~/Desktop/research/notes/R_code/makeDesign.R")
source("~/Desktop/research/notes/R_code/analyzeChips.R")


pl<-c("RO9", "ROonly", "mouse4302")
ctr.list<-pl[1]
gse<-pl[2]
chip<-pl[3]

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

#######  Step 3:  Fit linear model for each gene (exprSet => MArrayLM)     #####
   design.matrix <- makeDesign(eset,gsm2cond)
   fit1 <- lmFit(eset,design.matrix)
#   assign("fit1",fit1,position=1)
cat("Step 3 completed, lmFit done.\n")

#######  Step 4:  Create pairwise comparisons (M values)                   #####
   contrast.matrix <- makeContrasts(contrasts=ctr.list,levels=colnames(design.matrix))
   fit2 <- contrasts.fit(fit1,contrast.matrix)
#   assign("fit2",fit2,position=1)
cat("Step 4 completed, contrasts.fit done.\n")

#######  Step 5:  Get test statistics for pairwise comparisons (p values)  #####
   fit3 <- eBayes(fit2)
#   assign("fit3",fit3,position=1)
cat("Step 5 completed, eBayes done.\n")


# the following is taken from notes-2010-03-03
my.fit<-exprs(eset.gf)
my.fit<-addEGID(my.fit,"mouse4302")
my.fit <- my.fit[which(my.fit$egid!="NA"),]
nc <- ncol(my.fit)
colnames(my.fit)[3:nc] <- as.character(g2c$Condition)[match(colnames(my.fit)[3:nc], row.names(g2c))]
o <- order(as.character(colnames(my.fit)[3:nc]))+2
my.fit <- my.fit[,c(1,2,o)]
tab<-table(my.fit$egid)
tab<-tab[which(tab>1)]     # remove egids with 0-1 probeset

  switch( class(my.fit),
    character =     { my.fit <- do.eval(my.fit) }
    ExpressionSet = { my.fit <- exprs(my.fit) }
    MArrayLM =      { my.fit <- my.fit$coefficients }
  )
