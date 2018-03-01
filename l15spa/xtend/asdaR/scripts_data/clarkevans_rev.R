clarkevans <- function (X, correction = c("none", "Donnelly", "guard"), clipregion = NULL) 
{
    verifyclass(X, "ppp")
    W <- X$window
    area <- area.owin(W)
    npoints <- X$n
    intensity <- npoints/area
    if (npoints == 0) 
        return(NA)
    correction <- pickoption("correction", correction, c(none = "none", 
        Donnelly = "Donnelly", donnelly = "Donnelly", guard = "guard"), 
        multi = TRUE)
    if (("Donnelly" %in% correction) && (W$type != "rectangle")) 
        warning("Donnelly correction only available for rectangular windows")
    nndistX <- nndist(X)
    Dobs <- mean(nndistX)
    Dpois <- 1/(2 * sqrt(intensity))
    answer <- NULL
    if ("none" %in% correction) {
        Rnaive <- Dobs/Dpois
        answer <- c(answer, naive = Rnaive)
    }
    if ("Donnelly" %in% correction) {
        if (W$type == "rectangle") {
            perimeter <- 2 * (diff(W$xrange) + diff(W$yrange))
            Dedge <- Dpois + (0.0514 + 0.0412/sqrt(npoints)) * 
                perimeter/npoints
            Redge <- Dobs/Dedge
        }
        else Redge <- NA
        answer <- c(answer, edge = Redge)
    }
    if ("guard" %in% correction && !is.null(clipregion)) {
#        clip <- as.owin(clipregion)
        clipregion <- as.owin(clipregion)
	clip <- inside.owin(X$x, X$y, clipregion)
        Dguard <- mean(nndistX[clip])
        Rguard <- Dguard/Dpois
        answer <- c(answer, guard = Rguard)
    }
    return(answer)
}
