#source("~/Desktop/research/notes/R_code/handyFunctions.R")

do.eval <- function (x) {
  eval(parse(text=x)) 
}

mv <- function (old.name,new.name) { 
  assign(new.name,do.eval(old.name),pos=1) 
}

get.duped <- function(my.list) {
  subset(my.list,duplicated(my.list))
}

subset.grep <- function(pattern,my.list) {
 subset(my.list, grepl(pattern,my.list))
}

re.order <- function(my.list,sort.col) {
  my.list[order(my.list[,sort.col]),]
  # default is increasing order
}



common.set <- function(x) {
 # Usage: set.intersect <- common.set( list( set1,set2,set3 ))
 #        x is a *list* (not a vector) of sub lists
   len<-length(x)
   uni<-sapply(x,unique,SIMPLIFY=FALSE)
   tab<-table(unlist(uni))
   names(which(tab==len))
}

av <- function (x) { as.vector(x,mode="numeric") }
# shorthand, use to convert single row dataframe to numeric vector

iqrOverI <- function (I, na.rm = TRUE) {
    function(x) {
        if (na.rm)  x <- x[!is.na(x)]
        IQR(x) > I
    }
}

sdOverS <- function (S, na.rm = TRUE) {
  function(x) {
    if (na.rm)  x <- x[!is.na(x)]
    sd(x) > S
  }
}

allOverA <- function (A, na.rm = TRUE) {
# should be faster than min(x) > A since it can stop as soon as one item fails?
# don't know how this is implemented but system.time() records it faster...
  function(x) {
    if (na.rm)  x <- x[!is.na(x)]
    all(x) > A
  }
}

between <- function (x,y) {
# x can be a single value, vector, or matrix (will return same)
  stopifnot (length(y)==2)
  x > y[1] & x < y[2]  
}


in.between <- function (x,y) {  # inclusive version of between
  stopifnot (length(y)==2)
  x >= y[1] & x <= y[2]  # or should it be two && ?
}


# COMPARE WITH SUBSET()  ****
#  subset(tab,tab>2)) same as
#  tab[which(tab>2)]           # subset very slightly faster than which?

#my.get <- function (my.df, x, y, z=NULL) {
  # x = a column NAME or a NUMBER
  # y = the value to match
  # z = the columns to return
#  if (!is.null(z)) {  my.df[which(my.df[,x]==y),z]
#  }  else          {  my.df[which(my.df[,x]==y),]  }
#}

o.get <- function (x,o) {
  # o should be a quoted string naming the object to look in
  # x is the name to look for
  eval(parse(text=paste(o,"$`",x,"`",sep=''))) 
}

has.dim <- function(x) { length(dim(x)) > 0 }

pctOverlap <- function (x, y) { length(intersect(x,y))/length(union(x,y)) }

fishers.p <- function (p.vals) {
  x <- (-2)*sum(log(p.vals))
  pchisq(x,length(p.vals)-1,lower.tail=F)
}

gregexpr.ss<- function (pattern, string) {
# returns a data frame of the start&stop positions of each match and the matching substring
  greg <- gregexpr(pattern,string)
  locs <- greg[[1]][1:length(greg[[1]])]
  lens <- attr(greg[[1]],"match.length")
  ends <- locs + lens - 1
  df <- data.frame(start=locs, stop=ends)
  ss <- apply(df,1,function(x) substr(string,x[1],x[2]))
  cbind(df,substring=ss)
}

my.substr <- function (x,pat) {
  r <- regexpr(pat,x)[1]
  if (r>0) ss <- substr(x,1,r-1)
  else ss <- x
  ss
}
#returns the substring of x up to the first occurence of pattern pat, e.g.:
#> x
#[1] "104184.1_2.3"
#> my.substr("[.]",x)    # note: use [] to escape . (not \.)
#[1] "104184"

neweg2old <- function (x) {
# takes a list of new.eg.ids and removes the suffix to give regular eg ids
  gsub("[.].*","",x)
}
cust2eg <- function (x) {
# takes a list of new.eg.ids and removes the suffix to give regular eg ids
  gsub("_at","",x)
}

eg2sg <- function (id,id.list) {
  xx<-unique(subset(id.list,grepl(id,id.list)))
  lapply(xx,function(x){
    x<-strsplit(x,"[.]")[[1]][2]
    x<-strsplit(x,"_")[[1]]
  })
}

tryAgain <- function(x) {
# intended for use with scan(my.url) in case the server is temporarily out
  my.ans <- try(x,silent=T)
  if(inherits(my.ans,"try-error")) {
    Sys.sleep(5)
    my.ans <- try(x,silent=T)
  }  
  if(inherits(my.ans,"try-error")) {
    my.ans <- NULL
  } 
  return(my.ans) 
}

assign.save <- function(obj,pref,suff) {
   obj.nm <- paste(pref,suff,sep='.')
   assign(obj.nm,obj,pos=1)
   save(list=c(as.character(obj.nm)), file=paste(obj.nm,"RData",sep='.')) 
}

assign.write <- function(obj,pref,suff) {
   obj.nm <- paste(pref,suff,sep='.')
   assign(obj.nm,obj,pos=1)
   write.table(do.eval(obj.nm), file=paste(obj.nm,"txt",sep='.'), quote=F, sep='\t', row.names=F) 
}

# match(x,y)  same as   which(y %in% x)
# returns the index of x in y
