makeNewSets <- function (loc.table, x) {
  j <- 1
  groups <- list()
  grp <- j

  for (i in 2:nrow(loc.table)) {
    if ((loc.table$loc.start[i] - loc.table$loc.start[j]) < x) {
      grp <- c(grp,i)
    } else {
      groups <- c(groups,list(grp))
      grp <- i
    }
    j <- i
  }
  c(groups,list(grp))  # after the last iteration
}
