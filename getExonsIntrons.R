getIntronExonTable <- function (egid) {
  my.url <- paste( 
         "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gene&id=",
          egid, "&rettype=gene_table&retmode=text", sep='')
  my.txt <- scan(my.url,what="character",sep="\n",quiet=TRUE,strip.white=TRUE)
  my.txt <- subset(my.txt,grepl("^[0-9bp -]*$",my.txt))
  my.txt <- gsub(" +[0-9]+ bp","",my.txt)
  my.txt <- unlist(sapply(my.txt,strsplit,"  "),use.names=F)
  my.txt <- gsub(" ","",my.txt)
  my.tab <- matrix(c(my.txt,""),ncol=3,byrow=T)
  colnames(my.tab) <- c("Exon","Coding Exon","Intron")
  my.tab
}

getExonIntronTable <- function (rna, dna) {
# Given:    an rna (cdna) sequence and a dna sequence
# Returns:  a table of intron and exon boundaries 

  pa <- pairwiseAlignment(rna,dna)
  ca <- as.character(aligned(pa))
  ss <- strsplit(ca,"-+")[[1]]

  exs <- lapply(ss,
                function(x,y){   # y is the dna sequence
                  mp<-matchPattern(x,y)
                  c(start(mp), end(mp)) 
               },
               dna)
  dna.exs <- matrix(unlist(exs),ncol=2,byrow=T)

  dna.ins <- NULL
  for (i in 2:nrow(dna.exs)) { 
     intron <- c(dna.exs[i-1,2]+1, dna.exs[i,1]-1)
     dna.ins <- rbind(dna.ins,intron)
  }

  ei.tab <- rbind(dna.exs,dna.ins)
  rownames(ei.tab) <- c(paste("exon",1:nrow(dna.exs),sep="."), 
                        paste("intron",1:nrow(dna.ins),sep="."))
  colnames(ei.tab) <- c("start","end")
  ei.tab[order(ei.tab[,1]),]
}


