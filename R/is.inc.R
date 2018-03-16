is.inc <- function(x){
    if (length(x) <= 1) return(TRUE)
    y <- diff(x)
    all(y >= 0)
}
