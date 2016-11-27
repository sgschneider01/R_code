getRNASeq <- function (accnums) {
  seqs <- sapply(accnums, function(accnum) {
     gi <- suppressWarnings(getGI(accnum))
     url.beg <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nuccore&id="
     url.end <- "&rettype=fasta&retmode=xml"
     my.url <- paste(url.beg, gi, url.end, sep='')
     my.xml <- scan(my.url,what="character",sep="\n",quiet=TRUE)
     my.xml <- xmlParse(my.xml)
     xpathApply(my.xml, "//TSeq_sequence", xmlValue)[[1]]
  })
  seqs
}
