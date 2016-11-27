library(XML)

getXML <- function (geo.id) {
# Note: gse.id must be "GSE####" but return GSM and GPL values are not prefixed
  url.beg <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=gds&term="
  url.end <- "&retmax=5000&usehistory=y"
  my.url <- paste(url.beg,geo.id,url.end,sep='')
  my.txt <- scan(my.url,what="character",sep='\n',quiet=TRUE)
  my.xml <- xmlParse(my.txt)
  que.key <- xpathSApply(my.xml,"//QueryKey",xmlValue)
  web.env <- xpathSApply(my.xml,"//WebEnv",xmlValue)
  url.beg <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=gds&mode=file&report=docsum"
  url.end <- paste("&query_key=",que.key,"&WebEnv=",web.env,sep='')
  my.url <- paste(url.beg,url.end,sep='')
  my.txt <- scan(my.url,what="character",sep='\n',quiet=TRUE) 
  my.xml <- xmlParse(my.txt)
  return(my.xml)
}