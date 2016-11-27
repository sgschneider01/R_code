getR <- function (old.rows) {
   x<-as.vector(old.rows[1,],mode="numeric")
   y<-as.vector(old.rows[2,],mode="numeric")
   cor(x,y)
}