mult2 <- function(dat){
    ## mark twins, triplets, etc; 'dat' is 'births'
    ##For one woman:
    equal <- function(x, y){
        abs(x - y) < 0.02 # a week
    }
    onew <- function(age){
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
    
    multi2 <- numeric(NROW(dat))
    
    for (id in unique(dat$id)){
        age <- dat$age[dat$id == id]
        multi2[dat$id == id] <- onew(age)
    }
    dat$multi2 <- multi2
    dat
}