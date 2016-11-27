reannotate <- function (groups, egid.new) {

  for (i in 1:length(groups)) {
    x <- match(groups[[i]],egid.new[,"affy.id"])  
    # groups[[i]] is a list of affy ids
    egid.new[x,"egid"] <- paste(egid.new[x,"egid"],"i",sep='.')
  }

}

