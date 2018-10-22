get_fert <- function(){
    library(skum)
    fams <- unique(obs$id[obs$sex == "female" & obs$starttyp == 2 & obs$birthdate > 1821 & obs$birthdate < 1870])
    women <- obs[obs$id %in% fams, c("id", "civst", "birthdate", "enter", "exit", "pid", "sluttyp")]
    women$event <- women$sluttyp == 2
    women <- women[order(women$id, women$enter), ]
    women <- eha::age.window(women, c(15, 50))
    kids <- per[per$mid %in% women$id, c("mid", "id", "kon", "foddat")]
    names(kids) <- c("id", "ch.id", "sex", "ch.birthdate")
    indx <- match(kids$id, per$id)
    kids$marStart <- per$marStart[indx]
    kids$marEnd <- per$marEnd[indx]
    kids$birthdate <- per$foddat[indx]
    kids <- kids[order(kids$id, kids$ch.birthdate), ]
    ##list(women = women, kids = kids)
    weq <- kids$id[is.na(kids$ch.birthdate)]
    kids <- kids[!(kids$id %in% weq), ]
    kids <- skum::rc(kids)
    kids$civst <- "umnarried"
    kids
    ## To be continued!!
}