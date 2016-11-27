getAccNums <- function(egid) {
  my.url <- paste( 
   "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gene&id=", 
   egid,   
   "&rettype=full_report&retmode=text",sep="")
   
  x <- scan(my.url,what="character",sep="\n",quiet=TRUE)
  x <- subset(x, grepl("NM_.*NP",x) )  # only ones with known proteins
  x <- gsub("[.].*","",x)
  x <- gsub(" +","",x)
  return(x)
}