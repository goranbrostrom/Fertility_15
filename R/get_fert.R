get_fert <- function(){
    library(skum)
    fams <- unique(obs$id[obs$sex == "female" & obs$starttyp == 2 & obs$birthdate > 1821 & obs$birthdate < 1870])
    women <- obs[obs$id %in% fams, c("id", "civst", "birthdate", "enter", "exit", "pid", "sluttyp")]
    women$event <- women$sluttyp == 2
    women <- women[order(women$id, women$enter), ]
    women <- eha::age.window(women, c(15, 50))
    kids <- per[per$mid %in% women$id, c("mid", "id", "kon", "foddat")]
    names(kids) <- c("id", "ch.id", "sex", "ch.birthdate")
    ## Women without kids:
    nchw <- unique(women$id[!(women$id %in% kids$id)])
    wchless <- per[per$id %in% nchw, c("id", "foddat", "marStart", "marEnd", "frsbosdat")]
    wchless <- wchless[!is.na(wchless$frsbosdat), ]
    wchless <- wchless[wchless$frsbosdat - wchless$foddat > 5500, ]
    wchless$birthdate <- as.numeric(eha::toTime(wchless$foddat))
    wchless$foddat <- NULL
    wchless$enddate <- as.numeric(eha::toTime(wchless$frsbosdat))
    wchless$frsbosdat <- NULL
    wchless$ch.id <- NA
    wchless$sex <- NA
    wchless$ch.birthdate <- NA
    wchless$civst <- "unmarried"
    wchless$lopnr <- 1
    wchless$antrec <- 1
    wchless$parity <- 0
    wchless$exit <- wchless$enddate - wchless$birthdate
    wchless$enter <- 15
    wchless$event <- 0
    wchless$marStart <- eha::toTime(wchless$marStart)
    wchless$marEnd <- eha::toTime(wchless$marEnd)
    ##
    indx <- match(kids$id, per$id)
    kids$marStart <- per$marStart[indx]
    kids$marEnd <- per$marEnd[indx]
    kids$birthdate <- per$foddat[indx]
    kids$enddate <- per$frsbosdat[indx]
    kids <- kids[order(kids$id, kids$ch.birthdate), ]
    ##list(women = women, kids = kids)
    weq <- kids$id[is.na(kids$ch.birthdate)]
    kids <- kids[!(kids$id %in% weq), ]
    kids$civst <- "unmarried"

    kids <- skum::rc(kids)
    kids$parity <- kids$lopnr - 1
    ## Create 'enter' and 'exit' and 'event':
    kids$exit <- as.numeric(kids$ch.birthdate - kids$birthdate) / 365.25
    kids$event <- 1
    kids$enter <- c(15, kids$exit[-NROW(kids)])
    kids$enter[kids$lopnr == 1] <- 15
    ## apply toTime to dates:
    kids$ch.birthdate <- as.numeric(eha::toTime(kids$ch.birthdate))    
    kids$birthdate <- as.numeric(eha::toTime(kids$birthdate))
    kids$marStart <- as.numeric(eha::toTime(kids$marStart))
    kids$marEnd <- as.numeric(eha::toTime(kids$marEnd))
    kids$enddate <- as.numeric(eha::toTime(kids$enddate))
    ## To be continued!!
    weq <- ((!is.na(kids$marStart)) & (kids$marStart - kids$birthdate < 15)) | is.na(kids$enddate)
    kids <- kids[!weq, ]
    
    ## Add to kids the last, open interval (if any)
    foro <- kids[(kids$antrec == kids$lopnr) & kids$exit < 50, ]
    foro$antrec <- foro$antrec + 1
    foro$lopnr <- foro$lopnr + 1
    foro$enter <- foro$exit
    foro$exit <- pmin(50, foro$enddate - foro$birthdate) ## NOTE!
    foro$event <- 0
    foro$ch.id <- NA
    foro$sex <- NA
    foro$ch.birthdate <- NA
    foro$parity <- foro$parity + 1
    kids <- rbind(kids, foro)
    kids <- kids[order(kids$id, kids$parity), ]
    ## Now add the childless women:
    kids <- rbind(kids, wchless)
    ##list(kids = kids, wchless = wchless)
    kids <- eha::age.window(kids, c(15, 50))
    kids$civst[!is.na(kids$marStart) & kids$marStart < kids$exit + kids$birthdate] <- "married"
    kids$civst <- factor(kids$civst)
    kids$lopnr <- kids$antrec <- NULL
    kids <- kids[kids$exit > kids$enter, ]
    saveRDS(kids, file = "kids2.rds")
}