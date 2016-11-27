require(XML)            # xmlParse(), etc.

getExons <- function (egid,sp) {
#returns the exon locations in the TRANSCRIPT
  accnums <- switch(sp, "Mm" =  get(egid,org.Mm.egREFSEQ),
                       "Hs" =  get(egid,org.Hs.egREFSEQ) )
  accnums <- subset(accnums, grepl("NM",accnums))
  gis <- sapply(accnums, function(x)suppressWarnings(getGI(x)))

  exons <- lapply(gis, function (gi){
    url.beg <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nuccore&id="
    url.end <- "&rettype=native&retmode=xml"
    my.url <- paste(url.beg, gi, url.end, sep='')
    my.xml <- scan(my.url,what="character",sep="\n",quiet=TRUE)
    seq.x  <- xmlParse(my.xml)
  
    st<-xpathApply(seq.x, "//Seq-feat[.//Imp-feat_key='exon']//Seq-feat_location//Seq-interval_from", xmlValue)
    nd<-xpathApply(seq.x, "//Seq-feat[.//Imp-feat_key='exon']//Seq-feat_location//Seq-interval_to", xmlValue)

    exons<-data.frame(from=as.numeric(unlist(st)), to=as.numeric(unlist(nd)), stringsAsFactors=F)
    if (dim(exons)[1]>0)  {
      return (exons)
    } else {
      cat ("no exon information available, mapping STSs \n")
      return(getSTS(seq.x))
    }
  })

}


