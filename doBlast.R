getBlastHits <- function (egid,sp) {
  affx <- switch(sp, "Mm" = as.data.frame(mouse4302probe) ,
                     "Hs" = as.data.frame(hgu133plus2probe)  )
  ps   <- switch(sp, 
                "Mm" = get(get(egid,org.Mm.egSYMBOL),mouse4302ALIAS2PROBE),
                "Hs" = get(get(egid,org.Hs.egSYMBOL),hgu133plus2ALIAS2PROBE)  )

  seqs <- lapply(ps,function(p)subset(affx[,1],grepl(p,affx$Probe.Set.Name)))
  names(seqs) <- ps

  hits <- lapply(seqs,function(sq) lapply(sq,kaBoom,sp))
}

kaBoom <- function (my.seq,sp) {
  taxid <- switch(sp, "Hs" = 10090) ,
                      "Mm" = 9606)  )

  my.url  <- paste("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&", 
                   "PROGRAM=blastn&FILTER=L&HITLIST_SIZE=5&QUERY=",
                   my.seq,sep='')
  my.html <- scan(my.url,what="character",sep="\n",quiet=TRUE)
  my.rid  <- gsub(".* ","",subset(my.html,grepl("RID = ",my.html)))
  my.url  <- paste("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Get&", 
                   "FORMAT_TYPE=XML&RID=", my.rid, sep='')
  my.txt <- scan(my.url,what="character",sep="\n",quiet=TRUE)
  my.xml <- NULL
  while (is.null(my.xml)) {
    system("sleep 3")  # recommended by NCBI
    my.txt <- scan(my.url,what="character",sep="\n",quiet=TRUE)
    my.xml <- tryCatch( xmlParse(my.txt, asText = TRUE, error = NULL),
                        XMLError = function(e) {return(NULL)})
  }

#  h1 <- xpathApply(my.xml, "//Hit_accession", xmlValue)
#  h2 <- xpathApply(my.xml, "//Hit_id", xmlValue)
#  h3 <- xpathApply(my.xml, "//Hsp_evalue", xmlValue)
#  h4 <- xpathApply(my.xml, "//Hsp_positive", xmlValue)
#  res <- data.frame(hit_acc=unlist(h1), hit_id=unlist(h2), hsp_eval=as.numeric(unlist(h3)), hsp_pos=as.numeric(unlist(h4)))
#  hit <- subset(res$hit_acc, grepl("ref",res$hit_id) & res$hsp_eval<.01 & res$hsp_pos==25)
#  hit <- subset(res$hit_acc, res$hsp_eval<.01 & res$hsp_pos==25)
#  as.character(hit)

my.xml

}

