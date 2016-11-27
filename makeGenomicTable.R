source("~/Desktop/research/notes/R_code/getSeq.R")

#makeGenomicTable <- function (plat.name) {


  ps.tab <- do.eval(paste(plat.name,"probe",sep=''))
  eg.tab <- do.eval(paste(plat.name,"ENTREZID",sep=''))
  probe.set.names <- unique(ps.tab$Probe.Set.Name)

  new.tab <- NULL
# x<-1
x<-i
  for (i in x:length(probe.set.names)) {
    ps <- probe.set.names[i]
    egid <- get(ps,eg.tab)

    if (!is.na(egid)) {
      seqs <- subset(ps.tab$sequence,ps.tab$Probe.Set.Name == ps)
      dna  <- getDNASeq(egid)
      if (!is.null(dna)) {
        matches <- unlist(lapply(seqs, function(x){gregexpr(x,dna)[[1]][1]}))
        rng <- c(-1,-1)
        if (any(matches>0)) { rng <- range(subset(matches,matches>0)) }
      } else {
        rng <- c(NA,NA)
      }
      prbs.num <- sum(new.tab[,1]==as.numeric(egid),na.rm=T)+1
      new.line <- c(as.numeric(egid),prbs.num,rng)
    } else {
      new.line <- c(NA,NA,NA,NA)
    }
    new.tab <- rbind(new.tab,new.line) 
  }
  rownames(new.tab) <- probe.set.names
  colnames(new.tab) <- c("egid","ps.num","start","stop")


#  new.tab
#}

ps.tab<-setdiff(smallerplat,biggerplat)
and copy over intersection

#fixing error - did not have na.rm=T in prbs.num <- sum(new.tab[,1]==.....
#  for (i in 1:nrow(new.tab)) {
#    egid <- new.tab[i,1]
#    if (!is.na(egid)) {
#      new.tab[i,2] <- sum(new.tab[1:i,1]==as.numeric(egid),na.rm=T)
#    } #else leave it as NA
#  }