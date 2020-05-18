mult3 <- function(dat){
    ## mark twins, triplets, etc; 'dat' is 'births'
    ## Try 'tapply' here.
    ##For one woman:
    
    onew <- function(age){
        equal <- function(x, y){
            abs(x - y) < 0.02 # a week
        }
        n <- length(age)
        mul <- numeric(n)
        if (n <= 1L) return(mul)
        ##cat("n = ", n, "\n")

        if (equal(age[1], age[2])) mul[2] <- 1
        if (n >= 3){
            for (i in 3:n){
                if (equal(age[i], age[i-1])) mul[i] <- mul[i-1] + 1
            }
        }
        mul
    }
    
    dat$multi2 <- unlist(tapply(dat$age, dat$id, onew))
    dat
}