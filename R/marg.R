marg <- function(n = c(1000, 2000, 2000, 1000),
                 alpha = 1,
                 beta = 0.2,
                 cens = 1){
    ## n(i, j) = antal individer av kön i och behandling j.
    ## alpha = skillnad mellan kön ('sex').
    ## beta = behandlingseffekt ('year')
    ##
    ## Model: h(t) = h_0(t) exp(alpha * sex + beta * year)
    ## with h_0(t) == 1
    set.seed(108213)
    n <- matrix(n, ncol = 2, nrow = 2)
    
    d00 <- data.frame(enter = rep(0, n[1, 1]),
                      exit = rexp(n[1, 1]),
                      event = rep(1, n[1, 1]),
                      sex = rep(0, n[1, 1]),
                      year = rep(0, n[1, 1]))
                      
    d01 <- data.frame(enter = rep(0, n[1, 2]),
                      exit = rexp(n[1, 2], exp(beta)),
                      event = rep(1, n[1, 2]),
                      sex = rep(0, n[1, 2]),
                      year = rep(1, n[1, 2]))

    d10 <- data.frame(enter = rep(0, n[2, 1]),
                      exit = rexp(n[2, 1], exp(alpha)),
                      event = rep(1, n[2, 1]),
                      sex = rep(1, n[2, 1]),
                      year = rep(0, n[2, 1]))

    d11 <- data.frame(enter = rep(0, n[2, 2]),
                      exit = rexp(n[2, 2], exp(alpha + beta)),
                      event = rep(1, n[2, 2]),
                      sex = rep(1, n[2, 2]),
                      year = rep(1, n[2, 2]))

    dat <- rbind(d00, d01, d10, d11)
    dat$exit <- round(dat$exit, digits = 3)
    dat$exit <- ifelse(dat$exit < 0.001, 0.001, dat$exit)

    ## Censor at 'cens':
    library(eha)
    dat <- age.window(dat, c(0, cens))
    dat$event <- as.logical(dat$event)
    dat
}
    
    
