tryAgain <- function(x) {
# for use with scan(my.url) in case the server is temporarily not responding
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
