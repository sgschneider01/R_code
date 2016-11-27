parseExonTable <- function(egid) {
  my.url <- paste( 
   "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gene&id=", 
   egid,   
   "&rettype=gene_table&retmode=text",sep="")
   
  x <- tryAgain(scan(my.url,what="character",sep="\n",quiet=TRUE))
  x <- subset(x, grepl("^[0-9][0-9 bp-]*$",x) | grepl("^NM_.*number",x) )

  x <- gsub(" - ","-",x)            # remove extra spaces
  x <- gsub(" {5}","  none   ",x)   # replace missing values
  x <- gsub(" {3}[0-9]+ bp","",x)   # remove lengths
  x <- gsub("(.+  .+)  .+","\\1",x) # drop the introns

  g <- grep("NM",x)
  if (length(g)==0) return(NULL)

  gg <- c(g,length(x)+1)
  tab<-list()
  for (i in 1:(length(gg)-1) ) {
    tab[i] <- list(x[(gg[i]+1):(gg[i+1]-1)])
  }
  names (tab) <- x[g]

  tab <- lapply(tab,function(x) {
            mat <- sapply(x,strsplit," +")
            mat <- suppressWarnings(matrix(unlist(mat),ncol=2,byrow=T))
            cds <- unlist(sapply(mat[,2],strsplit,"-"))
            cds <- suppressWarnings(range(as.numeric(cds),na.rm=T))
            mat <- unlist(sapply(mat[,1],strsplit,"-"))
            mat <- matrix(as.numeric(mat),ncol=2,byrow=T)
            list(cds=cds,exons=mat)
         })
  return(tab)
}
