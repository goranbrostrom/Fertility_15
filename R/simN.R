simN <- function(h = 0.2, beta = 0.16, wait = 0,
                 max_parity = 100, silent = TRUE){
    ## simulating a process N(t) with h(t) = h * exp(beta * N(t-))
    ## with delay 'wait' after each birth.
    
    ## from 0 to 35:

    ebeta <- exp(beta)
    intensity <- h
    x <- rexp(1, intensity)
    parity <- 0
    b <- numeric(0)
    while (x < 35 & parity < max_parity){
        parity <- parity + 1
        b <- c(b, x)
        if (!silent & !(parity %% 10)){
            cat("parity = ", parity, "age = ", b[parity] + 15,
                "intensity = ", intensity, "\n")
        }
        start <- x
        intensity <- intensity * ebeta
        x <- b[parity] + wait + rexp(1, intensity)
    }
    
    ## Add 15 to get 'from 15 to 50': 
    invisible(list(ages = b + 15, intensity = intensity))
}
