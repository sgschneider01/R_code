source("~/Desktop/research/notes/R_code/addAnnotation.R")

getAnnEset <- function ( my.eset, exp.platform, drop.na=TRUE) {
   my.fit <- exprs(my.eset)
   my.fit <- addAnnotation(my.fit, exp.platform, "egid")
   if (drop.na) my.fit <- subset(my.fit, my.fit$egid!="NA")
   my.fit
}


getAnnFit <- function (my.fit, exp.platform) {
   my.fit <- my.fit$coefficients
   my.fit <- addAnnotation(my.fit, exp.platform, "egid")
   my.fit <- my.fit[which(my.fit$egid!="NA"),]
   my.fit
}

#   nc  <- ncol(my.fit)
#   g2c <- getConditions(exp.name)
#   ord <- match(rownames(g2c),colnames(my.fit))
#   my.fit <- my.fit[,c(1,2,ord)]
#   colnames(my.fit)[3:nc] <- as.character(g2c$Condition)

