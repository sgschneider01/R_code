addAnnot <- function (x=NULL,y) {
# x:  a dataframe to add the annotations to
# y:  what chip to use 
   
   source("~/Desktop/research/notes/R_code/loadAnnotations.R")
   if (!exists("mouse4302.keg")) loadAnnotations() 

   switch(y,
     mouse4302 = {
        if (is.null(x)) { x <- mouse4302.sym
        } else          { x <- merge(x, mouse4302.sym, all.x=TRUE, by="affy.id") }
        x <- merge(x, mouse4302.etg, all.x=TRUE, by="affy.id")
        x <- merge(x, mouse4302.keg, all.x=TRUE, by="affy.id")
   #    x <- merge(x, mouse4302.rfs, all.x=TRUE, by="affy.id")
     },
     hgu133a2 = {
        if (is.null(x)) { x <- hgu133a2.sym
        } else          { x <- merge(x, hgu133a2.sym, all.x=TRUE, by="affy.id") }
        x <- merge(x, hgu133a2.etg, all.x=TRUE, by="affy.id")
        x <- merge(x, hgu133a2.keg, all.x=TRUE, by="affy.id")
   #    x <- merge(x, hgu133a2.rfs, all.x=TRUE, by="affy.id")
     },
     hgu133plus2 = {
     # same as hgu133a2 for now
        if (is.null(x)) { x <- hgu133a2.sym
        } else          { x <- merge(x, hgu133a2.sym, all.x=TRUE, by="affy.id") }
        x <- merge(x, hgu133a2.etg, all.x=TRUE, by="affy.id")
        x <- merge(x, hgu133a2.keg, all.x=TRUE, by="affy.id")
   #    x <- merge(x, hgu133a2.rfs, all.x=TRUE, by="affy.id")
     }
   )

cat("Annotations are done.\n")
   return(x)
}