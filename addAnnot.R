#source("~/Desktop/research/notes/R_code/addAnnot.R")

addAnnot <- function (df,sp,field) {
# df:  a dataframe to add the annotations to
# sp:  what species to use 

# like addAnnotation, but for dataframes with EG ids or new (kw) ids, not affy 

require(org.Mm.eg.db)
require(org.Hs.eg.db)

  switch(field,
    symbol = {
      ann <- paste("org",sp,"egSYMBOL",sep='.')
      p <- as.list(do.eval(ann))
      p <- data.frame(eg.id=names(p),sym=as.character(p))
    },
    name = {
      ann <- paste("org",sp,"egGENENAME",sep='.')
      p <- as.list(do.eval(ann))
      p <- data.frame(eg.id=names(p),name=as.character(p))
    },
    {stop("you must specify a valid field")}
  )
  
  if (is.matrix(df)) {
    df<-data.frame(df)
  }
  if (sum(grepl("[_]",rownames(df)))>0) { 
    df<-cbind(eg.id=neweg2old(rownames(df)),new.id=(rownames(df)),df)
  }

  if ("eg.id" %in% colnames(df)) {
    df <- merge(p, df, all.y=TRUE)
  } else {
    df <- merge(p, df, all.y=TRUE, by.x="eg.id", by.y=0)
  }
  return(df)
}