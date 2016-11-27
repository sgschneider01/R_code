#source("~/Desktop/research/notes/R_code/annomaker.R")
ann.o.maker <- function (fldnm) {
# fldnm is a string indicating the field to use (e.g., "hgu133aSYMBOL")
# returns a data.frame with 2 columns, $affy.id and $fldnm

   fld <- as.list(eval(parse(text=fldnm)))
   for (i in 1:length(fld)) {
      if (length(fld[[i]])>1) 
         fld[[i]]<-paste(fld[[i]],sep=' ',collapse=' ')
   }
   fld <- unlist(fld)
   fld <- data.frame ( cbind(names(fld), as.character(fld)), row.names=NULL)
   colnames(fld) <- c("affy.id", fldnm)
   return(fld)
}


#fld<-data.frame(fldnm=as.character(fld),row.names=names(fld))
