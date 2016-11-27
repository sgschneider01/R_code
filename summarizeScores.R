summarizeScores <- function(exp,pkg) {
fit4 <- load(paste(exp,"/",exp,".fit4.RData",sep=""))
new.ids <- load(paste(exp,"/",exp,".new.ids.RData",sep=""))
dt <- load(paste(exp,"/",exp,".dt.RData",sep=""))
score.tab <- read.table(paste(pkg,"-scores.txt",sep=''),header=T,as.is=T)

# expects cols probe_id not affy.id, score not ?
# what about extra info? package?


  m.vals <- fit4$coefficients
  p.vals <- fit4$p.value
  m.vals <- m.vals * abs(dt)
  p.vals <- p.vals * abs(dt)
  comb.score <- m.vals * (1-p.vals)
  comb.score <- suppressWarnings(comb.score[which(apply(comb.score,1,any)),])
  
  new.ids <- subset(new.ids, new.ids$new.gene_id %in% rownames(dt))  


ids.scores <- merge(new.ids,probe.scores)
ids.scores <- tapply(ids.scores$score,ids.scores$score.id,mean)

  probe.scores <- tapply(as.character(new.ids$probe_id), 
      as.character(new.ids$new.gene_id), function(x) { mean(as.numeric(subset(score.tab$score,score.tab$probe_id %in% x))) })

  comb <- comb[names(probe.scores),]
  comb <- apply(comb,2,"*",probe.scores)
  return(comb)
}

# dt must be the direct result of decideTests
# fake score data:
#  score.tab <- data.frame(cbind(probe_id=as.character(new.ids$probe_id), 
#   score=c(rep(seq(.1,1,.1),nrow(new.ids)/10),.5,.5)),stringsAsFactors=F)



do experiment scores for each contrast in each experiment


cnds <- g2c$Condition
for (ctr in contrasts) {
  x <- strsplit(ctr,"-")[[1]]
  mean(c(sum(cnds==x[1]),sum(cnds==x[2])))
}  

# ctr.scores should be a vecor of length = ncol(f4)
final.score <- apply(f4score, 1, '*', ctr.scores)