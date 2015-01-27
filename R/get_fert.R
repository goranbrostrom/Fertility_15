get_fert <- function(){
    load("kids.rda")
    library(skel14)
    indx <- match(kids$ch.id, person$ID)
    kids$mb <- person$FB[indx]
    kids$sb <- person$DF[indx]
    dubbel <- unique(kids$id[kids$mb > 1.5])
    n_d <- length(dubbel)
    fert <- kids[!(kids$id %in% dubbel), ]
    short_ivl <- unique(fert$id[fert$exit - fert$enter < 0.76 & fert$parity > 0.5 & fert$event > 0.5])
    fert <- fert[!(fert$id %in% short_ivl), ]
    early <- unique(fert$id[fert$enter < 15.01])
    fert <- fert[fert$id %in% early, ]
    mid <- fert$id[fert$exit - fert$enter < 0.75 & fert$event > 0.5]
    fert <- fert[!(fert$id %in% mid), ]
    par0 <- fert$id[fert$parity == 0]
    fert <- fert[fert$id %in% par0, ]
    fert <- fert[fert$lopnr - fert$parity > 0.9 & fert$lopnr - fert$parity < 1.1, ]
    
    fert$sb <- fert$mb <- fert$lopnr <- NULL
    fert <- fert[order(fert$id, fert$parity),]
    rownames(fert) <- 1:NROW(fert)
    save(fert, file = "fert.rda")
    cat("wrote 'fert.rda' to disk")
    invisible(fert)
}
