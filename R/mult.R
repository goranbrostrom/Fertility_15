mult <- function(x){
    equal <- function(x, y){
        abs(x - y) < 0.02
    }
    
    n <- length(x)
    result <- numeric(n) # all ZERO
    
    if (n >= 2){
        if (equal(x[1], x[2])){
            result[1:2] <- 1:2
        }
        if (n > 2){    
            for (i in 3:n){
                if (equal(x[i], x[(i-1)])){
                    ##cat("result[(i-1)] = ", result[(i-1)], "\n")
                    ##cat("i = ", i, "\n")
                    ##cat(x, "\n")
                    ##cat(result, "\n")
                    if (result[(i-1)] == 0){
                        result[(i-1)] <- 1
                        result[i] <- 2
                    }else{  
                        result[i] <- result[i-1] + 1
                    }
                }
            }
        }
    }
    
    result
}