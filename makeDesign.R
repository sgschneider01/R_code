makeDesign <- function (eset,gsm2cond) {

#Takes:    a GSE (eset) object, and a gsm2cond object
#Returns:  the design.matrix, used by analyzeGEO.makeContrasts()

   design.matrix <- model.matrix(~ -1 + gsm2cond[sampleNames(eset),1])
   #    Reorders the rows to be in the same order as in the exprSet
   colnames(design.matrix) <- levels(gsm2cond[,1])
   #    levels (gsm2cond[,1]) = the condition names
   rownames(design.matrix) <- sampleNames(eset)

   design.matrix <- design.matrix[,which(apply(design.matrix,2,sum)>0)]
   # remove the columns with all zeros (conditions listed in the GSM-to-Condition file that are not used in this analysis)

   return(design.matrix)
}
