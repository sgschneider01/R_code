byGene <- function (my.fit, r.crit, chip) {
  source("~/Desktop/research/notes/R_code/getPairwiseMatrix.R")
  source("~/Desktop/research/notes/R_code/getRows.R")


  if (class(my.fit)=="ExpressionSet")
     my.fit <- exprs(my.fit) 
  if (class(my.fit)=="MArrayLM")
     my.fit <- my.fit$coefficients

  my.fit <- addEGID(my.fit,chip)
  my.fit <- my.fit[which(my.fit$egid!="NA"),]
  nc <- ncol(my.fit)

  colnames(my.fit)[3:nc] <- as.character(g2c$Condition)[match(colnames(my.fit)[3:nc], row.names(g2c))]
o <- order(as.character(colnames(my.fit)[3:nc]))+2
my.fit <- my.fit[,c(1,2,o)]

tab<-table(my.fit$egid)
tab<-tab[which(tab>1)]     # remove egids with 0-1 probeset



  tab <- table(my.fit$egid)
  singles <- names(tab)[which(tab==1)]
  new.data <- NULL
  new.data <- rbind(new.data, my.fit[match(singles,my.fit$egid),] )
  
cat("done with singles\n")  
  
  #deal with doubles
  doubles <- names(tab)[which(tab==2)]
  new.rows <- NULL
  for (d in doubles) {
     old.rows <- my.fit[which(my.fit$egid==d),3:nc]  
       #old.rows[,1]  is considered a data frame, won't work in cor.test
     x<-as.vector(old.rows[1,],mode="numeric")
     y<-as.vector(old.rows[2,],mode="numeric")

     r <- cor.test(x,y)$estimate
  }

cat("done with doubles\n")  

  multiples <- names(tab)[which(tab>2)]
  for (m in mults) {
    old.rows <- get.rows.as.cols(m,my.fit)
    mat <- getPairwiseMatrix(old.rows,method="spearman")
    w <- mean(mat)
    if (w>r.crit) {
       new.row  <- c(do.eval(m),apply(old.rows,2,mean),TRUE)
       new.data <- rbind(new.data, new.row )
    } else {
       cliqs <- getCliques(mat)
       cliqs <- lapply(cliqs,function(x){sub("n","",x)}) # get rid of "n" added by graphAM
       cliqs <- lapply(cliqs,as.numeric)

       if (any(duplicated(unlist(cliqs)))) {
         cat("overlaps")
       }

      for (cl in cliqs) {
         # what to do with a clique that is very low
         # what to do with overlapping cliques ie clique missing one edge
         p<-paste("FALSE",paste(cl,collapse=''),sep=":")
         if(length(cl)>1) {
           new.row  <- c(do.eval(m),apply(old.rows[,cl],2,mean),p)
         } else {
           new.row  <- c(do.eval(m),old.rows[,cl],p)
         }
         new.data <- rbind(new.data, new.row )
      }
    }
  }

cat(Sys.time())

return (new.data)

}