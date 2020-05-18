plotid <- function(dat, id = 160){
    enter <- dat$enter[fert$id == id]
    exit <- dat$exit[fert$id == id]
    n <- length(enter)
    plot(c(enter[1], exit[1]), c(0, 0), type = "l", col = "blue",
         xlim = c(15, 50), ylim = c(0, n), 
         xlab = "Age", ylab = "N(t)", axes = FALSE)
    axis(1, at = round(c(15, exit)), labels = round(c(15, exit)), 
         cex.axis = 0.85)
    axis(2)
    box()
    points(exit[1], 0, pch = "c", col = "blue")
    for(i in 2:n){
        lines(c(enter[i], exit[i]), c(i-1, i-1), col = "blue")
        points(exit[i], i-1, pch = "c", col = "blue")
    }
}
