getExpType <- function (gse.id) {
# Note: gse.id must be "GSE####" but return GSM and GPL values are not prefixed
  url.beg <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=gds"
  url.mid <- paste("&term=",gse.id,sep='')
  url.end <- "&retmax=5000&usehistory=y"
  my.url  <- paste(url.beg,url.mid,url.end,sep='')
  my.txt  <- scan(my.url,what="character",sep='\n',quiet=TRUE)
  my.xml  <- xmlParse(my.txt)

  que.key <- xpathSApply(my.xml,"//QueryKey",xmlValue)
  web.env <- xpathSApply(my.xml,"//WebEnv",xmlValue)

  url.beg <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=gds"
  url.end <- paste("&query_key=",que.key,"&WebEnv=",web.env,sep='')
  my.url <- paste(url.beg,url.end,sep='')
  my.txt <- scan(my.url,what="character",sep='\n',quiet=TRUE) 
  my.xml <- xmlParse(my.txt)

# <Item Name="entryType" Type="String">GSE</Item>
# <Item Name="gdsType" Type="String">Expression profiling by array</Item>
  exp.type <- xpathSApply(my.xml, "//DocSum[Item[@Name='entryType']='GSE']/Item[@Name='gdsType']", xmlValue )

# <Item Name="entryType" Type="String">GPL</Item>
# <Item Name="ptechType" Type="String">in situ oligonucleotide</Item>
  plat.num  <- xpathSApply(my.xml, "//DocSum[Item[@Name='entryType']='GPL']/Item[@Name='GPL']", xmlValue )
  plat.type <- xpathSApply(my.xml, "//DocSum[Item[@Name='entryType']='GPL']/Item[@Name='ptechType']", xmlValue )
  plat.titl <- xpathSApply(my.xml, "//DocSum[Item[@Name='entryType']='GPL']/Item[@Name='title']", xmlValue )
  plat.tax  <- xpathSApply(my.xml, "//DocSum[Item[@Name='entryType']='GPL']/Item[@Name='taxon']", xmlValue )

  return(c(exp.type,plat.num,plat.type,plat.titl,plat.tax))
}

