summarizeChips <- function (gse.nm) {

#Takes:    the name of a GSE (eset) object
#Uploads:  a gsm2cond file, if the object (gse.nm.g2c) doesn't already exist
#Creates:  the g2c object, if it doesn't already exist
#          the design.matrix, used by analyzeGEO.makeContrasts()
#Returns:  an lmFit object (average expression for each probeset across all replicate chips (chips having the same conditions))

   eset <- eval(parse(text=gse.nm))

   g2c <- paste(gse.nm,".g2c",sep='')
   if (!exists(g2c)) {
      cat("  Getting GSM-to-Condition file\n")
      g <- strsplit(gse.nm,"_")[[1]][1]   # remove trailing _x eg gse.nm3834_t
      fn <- paste("~/Desktop/research/geo-analysis/",g,".txt",sep='')
      gsm2cond <- read.table(file=fn) 
      gsm2cond <- gsm2cond[which(!is.na(match(rownames(gsm2cond),gsm.list))), 1, drop=FALSE]
      #  remove the rows not used in this analysis
      assign (g2c,gsm2cond, pos=1)
   } else {
      gsm2cond <- eval(parse(text=g2c))
   }

cat("  Creating the lmFit object for ",gse.nm,".\n")

   design.matrix <- model.matrix(~ -1 + gsm2cond[sampleNames(eset),1])
   #    Reorders the rows to be in the same order as in the exprSet
   colnames(design.matrix) <- levels(gsm2cond[,1])
   #    levels (gsm2cond[,1]) = the condition names
   rownames(design.matrix) <- sampleNames(eset)

   design.matrix <- design.matrix[,which(apply(design.matrix,2,sum)>0)]
   # remove the columns with all zeros (conditions listed in the GSM-to-Condition file that are not used in this analysis)

   assign("design.matrix",design.matrix,pos=1)
   #create it in the global scope to be used by analyzeGEO.makeContrasts

   lm.marray <- lmFit(eset,design.matrix)
   return (lm.marray)  #will be fit1
}
