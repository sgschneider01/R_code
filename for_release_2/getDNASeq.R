getDNASeq <- function (gene_id) {
  url.beg <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gene&id="
  url.end <- "&retmode=xml"
  my.url  <- paste(url.beg, gene_id, url.end, sep='')

  my.xml <- suppressWarnings(tryAgain(scan(my.url,what='character',sep='\n', quiet=TRUE)))
  
  if (!is.null(my.xml)) {
    my.xml <- xmlParse(my.xml)
  
    path <- "//Gene-commentary[Gene-commentary_type[@value='genomic']][1]/Gene-commentary_seqs"
    st  <- xpathApply(my.xml, paste(path,"//Seq-interval_from",sep=''), xmlValue)
    nd  <- xpathApply(my.xml, paste(path,"//Seq-interval_to",sep=''), xmlValue)
    gi  <- xpathApply(my.xml, paste(path,"//Seq-id_gi",sep=''), xmlValue)
    str <- xpathApply(my.xml, paste(path,"//Na-strand",sep=''), xmlGetAttr,"value")

    #  st <- as.numeric(st) - 5000
    #  nd <- as.numeric(nd) + 5000

    if (length(st)>0 & length(nd)>0 & length(gi)>0 & length(str)>0) {
      # length(x)>0 returns false if x=list() or x=NULL (but not x=NA)
      str <- switch(str[[1]], plus = 1, minus = 2)
      url.beg <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nuccore"
      url.mid <- paste("&id=",gi[[1]],"&strand=",str,"&seq_start=",st[[1]],"&seq_stop=",nd[[1]],sep='')
      url.end <- "&rettype=fasta&retmode=text"
      my.url  <- paste(url.beg, url.mid, url.end, sep='')

      my.fasta <- suppressWarnings(tryAgain(scan(my.url,what='character',sep='\n',quiet=TRUE)))
      if (!is.null(my.fasta)) {
        dna.seq  <- paste(my.fasta[-1],collapse="")
        return(dna.seq)
      }
    } 
  }
  return(NULL)  # for all 3 else conditions
}
