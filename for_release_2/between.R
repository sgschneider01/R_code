between <- function (x,y) {
# x can be a single value, vector, or matrix (will return same)
# y must be a vector of length 2 (the two values to check against)
  stopifnot (length(y)==2)
  x > y[1] & x < y[2]  
}
