to.model <- function (df) {
   #same as: as.data.frame.table(), which calls expand.grid()
   #         xtabs() is the reverse, but needs t() also 
   y <- unlist(c(df),use.names=F)  # goes by column
   x <- gl(n=ncol(df), k=nrow(df), length=length(df), labels=colnames(df))
   z <- gl(n=nrow(df), k=1,        length=length(df), labels=rownames(df))

   mf <- data.frame(x,z,y)
   mf
}
