legend_image <- function (x.leg, y.leg, values, scale.vals, vertical = FALSE, 
    offset.leg = 1, ...) 
{
    values <- values[!is.na(values)]
    if (length(x.leg) != 2 | length(y.leg) != 2) 
        stop("x.leg and y.leg require a vector with 2 elements")
    v.r <- range(values[is.finite(values)], na.rm = TRUE)
    lags.x <- function(xs, nl) {
        xs.r <- 0.5 * diff(xs/(nl - 1))
        return(seq(xs[1] + xs.r, xs[2] - xs.r, l = nl))
    }
    leg.l <- list(...)
    if (is.null(leg.l$br)) 
        nc <- ifelse(is.null(leg.l$col), 12, length(leg.l$col))
    else nc <- length(leg.l$breaks) - 1
    if (is.null(leg.l$col)) 
        leg.l$col <- heat.colors(nc)
    if (is.null(leg.l$zl)) 
        leg.l$zlim <- c(v.r[1], v.r[2])
    if (vertical) {
        xy <- list(x = x.leg, y = lags.x(xs = y.leg, nl = nc))
        if (is.null(leg.l$br)) 
            image(x = xy$x, y = xy$y, z = matrix(seq(leg.l$zlim[1], 
                leg.l$zlim[2], l = nc), nrow = 1), add = TRUE, 
                xaxs = "i", yaxs = "i", xlab = "", ylab = "", 
                zlim = leg.l$zlim, col = leg.l$col)
        else image(x = xy$x, y = xy$y, z = matrix(seq(leg.l$zlim[1], 
            leg.l$zlim[2], l = nc), nrow = 1), add = TRUE, xaxs = "i", 
            yaxs = "i", xlab = "", ylab = "", zlim = leg.l$zlim, 
            col = leg.l$col, breaks = leg.l$br)
    }
    else {
        xy <- list(x = lags.x(xs = x.leg, nl = nc), y = y.leg)
        if (is.null(leg.l$br)) 
            image(x = xy$x, y = xy$y, z = matrix(seq(leg.l$zlim[1], 
                leg.l$zlim[2], l = nc), ncol = 1), add = TRUE, 
                xaxs = "i", yaxs = "i", xlab = "", ylab = "", 
                zlim = leg.l$zlim, col = leg.l$col)
        else image(x = xy$x, y = xy$y, z = matrix(seq(leg.l$zlim[1], 
            leg.l$zlim[2], l = nc), ncol = 1), add = TRUE, xaxs = "i", 
            yaxs = "i", xlab = "", ylab = "", zlim = leg.l$zlim, 
            col = leg.l$col, breaks = leg.l$br)
    }
    leg.poly <- rbind(c(x.leg[1], y.leg[1]), c(x.leg[2], y.leg[1]), 
        c(x.leg[2], y.leg[2]), c(x.leg[1], y.leg[2]), c(x.leg[1], 
            y.leg[1]))
    polygon(leg.poly)
    if (is.null(leg.l$cex)) 
        leg.l$cex <- 0.8
    if (missing(scale.vals)) 
        scale.vals <- pretty(c(values, leg.l$zlim), n = 5, min.n = 4)
    scale.vals <- scale.vals[scale.vals > leg.l$zlim[1] & scale.vals < 
        leg.l$zlim[2]]
    if (vertical) {
        y.r <- range(lags.x(xs = y.leg, nl = nc))
        y.text <- y.r[1] + ((scale.vals - leg.l$zlim[1]) * diff(y.r))/diff(leg.l$zlim)
        text((max(x.leg) + offset.leg * diff(x.leg)), y.text, 
            lab = scale.vals, col = 1, cex = leg.l$cex)
    }
    else {
        x.r <- range(lags.x(xs = x.leg, nl = nc))
        x.text <- x.r[1] + ((scale.vals - leg.l$zlim[1]) * diff(x.r))/diff(leg.l$zlim)
        text(x.text, (max(y.leg) + offset.leg * (diff(y.leg)/2)), 
            lab = scale.vals, col = 1, cex = leg.l$cex)
    }
    return(invisible())
}
