library(XML)

getGSMlist <- function (gse.id) {

  base.id <- gsub("GSE","",gse.id)
  ohs <- paste(rep("0", 8-nchar(base.id)),collapse='')
  uid <- paste("2", ohs, base.id, sep='')
  url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=gds&id="
  my.url <- paste(url,uid,sep='')
  my.txt <- scan(my.url,what="character",sep='\n',quiet=TRUE)
  my.xml <- xmlParse(my.txt)

  my.ids <- xpathSApply(my.xml,"//Item[@Name='Sample']/Item[@Name='Accession']", xmlValue)

  my.conds <- xpathSApply(my.xml,"//Item[@Name='Sample']/Item[@Name='Title']", xmlValue)

  gsm.ids <- data.frame(ids=my.ids, Condition=my.conds)
  return(gsm.ids)
}


