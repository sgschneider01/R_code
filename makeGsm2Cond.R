library(XML)

# replaced with getGSMlist

makeGsm2Cond <- function (gse.id) {
# Note: gse.id must be "GSE####" but return GSM and GPL values are not prefixed

# esearch will return list of ids and web.env and query.key
  url.beg <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=gds"
  url.mid <- paste("&term=",gse.id,sep='')
  url.end <- "&retmax=5000&usehistory=y"
  my.url <- paste(url.beg,url.mid,url.end,sep='')
  my.txt <- scan(my.url,what="character",sep='\n',quiet=TRUE)
  my.xml <- xmlParse(my.txt)

  my.ids <- xpathSApply(my.xml,"//Id",xmlValue)
  my.gpl.ids <- gsub("^10+","GPL",subset(my.ids, grepl("^100[0-9]{3,}",my.ids)))

  que.key <- xpathSApply(my.xml,"//QueryKey",xmlValue)
  web.env <- xpathSApply(my.xml,"//WebEnv",xmlValue)

  url.beg <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=gds"
  url.end <- paste("&query_key=",que.key,"&WebEnv=",web.env,sep='')
  my.url <- paste(url.beg,url.end,sep='')
  my.txt <- scan(my.url,what="character",sep='\n',quiet=TRUE) 
  my.xml <- xmlParse(my.txt)

  accs <- xpathSApply(my.xml, "//DocSum[starts-with(Id,'300')]/Id", xmlValue )
  accs <- gsub("^30+","GSM",accs)   # different order than above!!!
  ttls <- xpathSApply(my.xml, "//DocSum[starts-with(Id,'300')]/Item[@Name='title']", xmlValue )
  sums <- xpathSApply(my.xml, "//DocSum[starts-with(Id,'300')]/Item[@Name='Characteristics']", xmlValue )
  # note case change (Title vs title) vs above
  gsm2cond <- cbind(accs, ttls, sums)
  #colnames(gsm2cond) <- c("ids","Cy3","Cy5")

  if (length(my.gpl.ids)>1) {
    gpls <- xpathSApply(my.xml, "//DocSum[starts-with(Id,'300')]/Item[@Name='GPL']", xmlValue )
    gpls <- paste("GPL",gpls,sep='')
    gsm2cond<-cbind(gsm2cond,gpls) 
 #   colnames(gsm2cond) <- c(colnames(gsm2cond),"GPL")
  }
  
  assign.write(gsm2cond,gse.id,"gsm2cond")
  return(my.xml)
}