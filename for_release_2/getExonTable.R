require(XML)

getExonTable <- function(egid) {
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


y <- subset(x,grepl("^[0-9]+[-][0-9]+\t",x) | grepl("Exon +table +for +mRNA",x))
y <- unlist(lapply(y,function(x)gsub("\t.*","",x)))
y <- unlist(lapply(y,function(x)gsub(".*NM_","NM_",x)))
y <- unlist(lapply(y,function(x)gsub(" .*","",x)))

> grep("NM",y)
[1]  1 16 32 49


ind <- grep("NM",y)
lap <- NULL
for (i in 1:length(ind)) {
   j <- ind[i]+1
   k <- ind[i+1]-1
   if(is.na(k)) k <- length(y)
   lap <- c(lap,list(y[j:k]))
}
names(lap)<-y[ind]