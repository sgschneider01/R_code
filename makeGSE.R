#source("~/Desktop/research/notes/R_code/makeGSE.R")

makeGSE <- function (gsmlist) {

# Takes:    a list of GSM ids that make up a GSE
# Creates:  the gsm objects, if they don't already exist
# Returns:  the GSE (eset) object

# Note: GSE files include GPL which is too big to download, 
#       so we download individual GSM files and combine 
#       (GSM files are already normalized so we don't have to use gcrma)

require(GEOquery)
gsmlist<-as.character(gsmlist)
   for (g in gsmlist) {  
      if (!exists(g)) {
        g.file <- paste(g,"RData",sep='.')
        if (file.exists(g.file)) {
          load(g.file)
        } else {
          assign(g, getGEO(g), pos=1)
          save(list=c(as.character(g)),file=g.file)
        }
      }
   }

   ids <- Table(eval(parse(text=gsmlist[1])))$ID_REF
   
   data.matrix <- 
      do.call("cbind",lapply(gsmlist,
         function(x) {
            tab <- Table(eval(parse(text=x)))
            tab$VALUE <- as.numeric(tab$VALUE)
            if (max(tab$VALUE,na.rm=T)>100) { 
               tab$VALUE <- pmax(tab$VALUE,rep(2,length(tab$VALUE))) # 4? 4.5?
               tab$VALUE <- log2(tab$VALUE)
            }
            mymatch<-match(ids,tab$ID_REF)
            return(tab$VALUE[mymatch]) 
         }
      ))
      #cbind makes a matrix of all the VALUES in each gsmfile (columns)
      #function(x) turns the data into a table and reorders the rows by "ids" 
      #   (the order of the first gsmfile in the list)
   rownames(data.matrix) <- ids
   colnames(data.matrix) <- gsmlist
 
   pdata <- data.frame(samples=gsmlist)
   rownames(pdata) <- gsmlist
   pheno <- as(pdata, "AnnotatedDataFrame")

   eset <- new("ExpressionSet",exprs=data.matrix,phenoData=pheno)
   return(eset)
}