sdOverS <- function (S, na.rm = TRUE) {
  function(x) {
    if (na.rm)  x <- x[!is.na(x)]
    sd(x) > S
  }
}
