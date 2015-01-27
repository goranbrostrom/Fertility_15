sim <- function(rate, frail = FALSE, pois = true, covar,
                alpha = 0, beta = 0){
    ## Simulera födslar i intervallet 15-50 för en kvinna.
    ## Givet fertiliteten i 'rate': styckevis konstant, 15-16, 16-17, ...
    ##
    ## Modell: h_0(t, x) = rate * exp(beta * covar + alpha * N(t-))

    library(eha)
    parity <- 0
    rate <- rate * exp(beta * covar + alpha * parity)
    ## Generera första
    x <- rpch(1, cuts = 1:34, levels = rate)
    if (x < 35){
        event <- 1
        exit <- x
        enter <- 0
        dat <- data.frame(enter = enter, exit = exit, event = event,
                          parity = 0, x = covar)
        done <- FALSE
        while (!done){
            enter <- exit + 0.75
            
            
    
