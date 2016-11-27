findORF <- function(seq) {
  require(Biostrings)
  if (!is(seq,"XString")) seq<-DNAString(seq)
  l<-nchar(seq)
  aa1<-suppressWarnings(translate(seq))
  ss1<-grepl("[*]",as.character(aa1))
  aa2<-suppressWarnings(translate(seq))
  ss2<-grepl("[*]",as.character(aa2))
  aa3<-suppressWarnings(translate(seq))
  ss3<-grepl("[*]",as.character(aa3))

  c(!ss1,!ss2,!ss3)  #if we found *, we do NOT have an ORF
}