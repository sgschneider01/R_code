getSTS <- function (seq.x) {
  require(XML)            # xmlParse(), etc.

  st<-xpathApply(seq.x, "//Seq-feat[.//Imp-feat_key='STS']//Seq-interval_from", xmlValue)
  nd<-xpathApply(seq.x, "//Seq-feat[.//Imp-feat_key='STS']//Seq-interval_to", xmlValue)

  sts <- data.frame( from=as.numeric(unlist(st)), to=as.numeric(unlist(nd)), stringsAsFactors=F)
  if (dim(sts)[1]>0)  {
    return (sts)
  } else {
     cat ("no STS information available\n")
     return(NULL)
  }
}
