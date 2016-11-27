checkDoubles <- function (old.rows, r.crit) {
  ids <- old.rows[,1:2]
  if (r < r.crit) {
    ids$egid[1] <- paste(ids$egid[1],1,sep='.')
    ids$egid[2] <- paste(ids$egid[2],2,sep='.')
  }
  ids
}
