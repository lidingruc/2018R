## Constants
A <- 6378245.0
EE <- 0.00669342162296594323
XM_PI <- pi * 3000.0 / 180.0

atn2 <- function(y, x){
    ## UDF
    if (x > 0) {
        return(atan(y / x))
    }else if (x < 0) {
        return(sign(y) * (pi - atan(abs(y / x))))
    }else if (y == 0){
        return(0)
    }else{
        return(sign(y) * pi / 2.0)
    }
}

isOutOfChina <- function(Lat, Lon){
    # to check if the plase is out of China
    OutOfChina <- FALSE
    if (Lon < 72.004 | Lon > 137.8347) OutOfChina <- TRUE
    if (Lat < 0.8293 | Lat > 55.8271) OutOfChina <- TRUE
    return(OutOfChina)
}

transformLat <- function(x, y){
    # China encrpytion of latitudes
    ret <- -100.0 + 2.0 * x + 3.0 * y + 0.2 * y * y + 0.1 * x * y + 0.2 * sqrt(abs(x))
    ret <- ret + (20.0 * sin(6.0 * x * pi) + 20.0 * sin(2.0 * x * pi)) * 2.0 / 3.0
    ret <- ret + (20.0 * sin(y * pi) + 40.0 * sin(y / 3.0 * pi)) * 2.0 / 3.0
    ret <- ret + (160.0 * sin(y / 12.0 * pi) + 320.0 * sin(y * pi / 30.0)) * 2.0 / 3.0
    return(ret)
}

transformLon <- function(x, y){
    # China encryption of longitudes
    ret <- 300.0 + x + 2.0 * y + 0.1 * x * x + 0.1 * x * y + 0.1 * sqrt(abs(x))
    ret <- ret + (20.0 * sin(6.0 * x * pi) + 20.0 * sin(2.0 * x * pi)) * 2.0 / 3.0
    ret <- ret + (20.0 * sin(x * pi) + 40.0 * sin(x / 3.0 * pi)) * 2.0 / 3.0
    ret <- ret + (150.0 * sin(x / 12.0 * pi) + 300.0 * sin(x / 30.0 * pi)) * 2.0 / 3.0
    return(ret)
}

getCoordArgs <- function(y, ...){
    ## Coarse params to a 2-col data.frame
    if (is.null(dim(y))){  ## vector or list
        if (is.list(y)) {
            y <- try(sapply(y, function(l) as.numeric(l[1:2])), silent=TRUE)
            if (! class(y) == 'try-error')
                out <- as.data.frame(y, stringsAsFactors=FALSE)
        }else{
            y <- as.numeric(y)
            if (length(y) == 1){
                ## y refers to a coordinate
                ## return one point
                x <- list(...)
                if (! length(x) == 0) {
                    x <- as.numeric(x)[[1]][1]
                }else{
                    x <- NA
                }
                out <- data.frame(lat=y, lon=x)
            }else if (length(y) >= 2) {
                ## y refers to one point
                ## return all possible points
                x <- list(...)
                if (length(x) >= 1){
                    x <- t(sapply(x, function(l) as.numeric(l[1:2])))
                    out <- rbind(y[1:2], as.data.frame(x))
                    out <- as.data.frame(out, stringsAsFactors=FALSE)
                }else{
                    out <- as.data.frame(t(y[1:2]))
                }
            }
        }
    }else{  ## matrix or data.frame
        if (is.data.frame(y)){
            ## get the first two columns
            y <- y[,1:2]
            y <- as.data.frame(sapply(y, as.character, simplify=FALSE),
                               stringsAsFactors=FALSE)
            y <- as.data.frame(sapply(y, as.numeric, simplify=FALSE))
            out <- y
        }else if (is.matrix(y)){
            if (ncol(y) == 1 || nrow(y) == 1){
                y <- as.vector(y)
                if (length(y) ==1) {
                    out <- data.frame(lat=NA, lon=NA)
                }else{
                    out <- data.frame(lat=y[1], lon=y[2])
                }
            }else{
                stopifnot(all(abs(as.vector(y)) <= 180, na.rm=TRUE))
                colcat <- rowcat <- vector(length=2)
                colcat[1] <- ifelse(any(abs(y[, 1]) > 90, na.rm=TRUE), 'lon', 'lat')
                colcat[2] <- ifelse(any(abs(y[, 2]) > 90, na.rm=TRUE), 'lon', 'lat')
                rowcat[1] <- ifelse(any(abs(y[1, ]) > 90, na.rm=TRUE), 'lon', 'lat')
                rowcat[2] <- ifelse(any(abs(y[2, ]) > 90, na.rm=TRUE), 'lon', 'lat')
                if (colcat[1] == colcat[2] && rowcat[1] == rowcat[2])
                    stop("Cannot distinguish lat and lon in the matrix either col 1-2 or row 1-2.")

                if (colcat[1] != colcat[2]){
                    out <- data.frame(as.numeric(y[,1]), as.numeric(y[,2]))
                    names(out) <- colcat
                }else{
                    out <- data.frame(as.numeric(y[1,]), as.numeric(y[2,]))
                    names(out) <- rowcat
                }
                out <- out[,c("lat", "lon")]
            }
        }
    }
    if (!is.null(out)){
        names(out) <- c('lat', 'lon')
        out[is.na(out$lat) | is.na(out$lon), ] <- c(NA, NA)
        if (any(abs(out['lat']) > 90 & abs(out['lon']) > 180, na.rm=TRUE))
            stop("Lat should be within [-90, 90], Lon should be within [-180, 180].")
        return(out)
    }
}

#' Transform WGS-84 (Global) coordinates to GCJ-02 (Google CN/Amap)
#'
#' WGS-84 coordinates are gloabal coordinates.
#' This function encrypts it into GCJ-02 that is mandated by Chinese Gov't.
#'
#' @aliases wgs2gcj
#' @note Latitude is the horizontal line serving as y-axis metric, longitude is
#' the vertical line serving as x-axis metric.
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param y \itemize{
#' \item A vector \code{c(latitude, longitude)} \cr
#' \item simply latitude number \cr
#' \item a matrix (row 1-2 or col 1-2). The function will choose how to read the data \cr
#' \item a data.frame (col 1-2)
#' }
#' @param ... \itemize{
#' \item When \code{y} is only latitude, you can pass in \code{x} (longitude) here. \cr
#' \item when \code{y} is a vector of \code{c(lat, lon)}, you can pass in the rest vectors as well.
#' \item when \code{y} is a matrix or a data.frame, ... is omitted.
#' }
#'
#' @seealso \code{\link{convGCJ2WGS}}, \code{\link{convCoord}}.
#' @references
#' \url{https://on4wp7.codeplex.com/SourceControl/changeset/view/21483#353936}
#' @return A 2-col data.frame ([lng, lat]) of transformed coordinates.
#' @export
#'
#' @examples
#' \dontrun{
#' # Tiananmen square's WGS-84 coordinate is c(39.90734, 116.39089)
#' # http://www.google.cn/maps/place/Tiananmen,+Dongcheng,+Beijing/@39.90874,116.39713,16z?hl=en
#'
#' ## Single point
#' convWGS2GCJ(c(39.90734, 116.39089))  # or
#' convWGS2GCJ(39.90734, 116.39089)  # get
#' #           lat      lng
#' # [1,] 39.90874 116.3971
#'
#' ## Multiple points
#' ### Vectors
#' convWGS2GCJ(c(39.90734, 116.39089), c(39.90734, 116.39089))  # get
#' #           lat      lng
#' # [1,] 39.90874 116.3971
#' # [2,] 39.90874 116.3971
#'
#' ### Matrix
#' m <- matrix(c(39.90734, 116.39089, 39.90734, 116.39089, 39.90734, 116.39089), nrow=2)
#' m
#' #           [,1]      [,2]      [,3]
#' # [1,]  39.90734  39.90734  39.90734
#' # [2,] 116.39089 116.39089 116.39089
#' convWGS2GCJ(m)  # get
#' #           lat      lng
#' # [1,] 39.90874 116.3971
#' # [2,] 39.90874 116.3971
#' # [3,] 39.90874 116.3971
#'
#' ### data.frame
#' df <- data.frame(lat=c(39.90734, 39.90734, 39.90734, NA),
#'                  lon=c(116.39089, 116.39089, 116.39089, 116.39089))
#' convWGS2GCJ(df)  # get
#' #           lat      lng
#' # [1,] 39.90874 116.3971
#' # [2,] 39.90874 116.3971
#' # [3,] 39.90874 116.3971
#' # [4,]       NA       NA
#' }
#'
convWGS2GCJ <- function(y, ...){
    # Coord Global GeoSystem (WGS-84) -> Mars GeoSystem (GCJ-02)
    # coords must be a vector c(lat,lon)
    input <- getCoordArgs(y, ...)
    if (! all(abs(input['lat']) <= 90 & abs(input['lon'] <= 180), na.rm=TRUE)){
        stop(paste0("Latitude should be within [-90, 90] and longitude ",
                    "should be within [-180, 180]."))
    }
    output <- as.data.frame(t(apply(input, MARGIN=1, .wgs2gcj)))
    names(output) <- c("lat", "lng")
    return(output)
}

#' @rdname convWGS2GCJ
#' @export
wgs2gcj <- convWGS2GCJ

.wgs2gcj <- function(coord, ...){
    if (length(coord) > 1){
        wgLat <- coord[1]
        wgLon <- coord[2]
    }else{
        wgLat <- coord
        wgLon <- unlist(list(...))[1]
    }

    if (is.na(wgLat) || is.na(wgLon)){
        return(c(NA, NA))
    }else{
        outOfChina <- isOutOfChina(wgLat, wgLon)
        if (!outOfChina){
            dLat <- transformLat(wgLon - 105.0, wgLat - 35.0)
            dLon <- transformLon(wgLon - 105.0, wgLat - 35.0)
            radLat <- wgLat / 180.0 * pi
            magic <- sin(radLat)
            magic <- 1.0 - EE * magic ** 2
            sqrtMagic <- sqrt(magic)
            dLat <- (dLat * 180.0) / ((A * (1.0 - EE)) / (magic * sqrtMagic) * pi)
            dLon <- (dLon * 180.0) / (A / sqrtMagic * cos(radLat) * pi)
            WGS2GCJ <- c(lat = wgLat + dLat, lon = wgLon + dLon)
        }else{
            WGS2GCJ <- c(lat = wgLat, lon = wgLon)
        }
    }
    return(WGS2GCJ)
}


#' Transform GCJ-02 (Google CN/Amap) coordinates to WGS-84 (Global)
#'
#' GCJ-02 is coordinate system mandated by Chinese Gov't.
#' This function decrypts it back to WGS-84 coordinates (gloabal coordinates).
#'
#' @note Latitude is the horizontal line serving as y-axis metric, longitude is
#' the vertical line serving as x-axis metric.
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param y \itemize{
#' \item A vector \code{c(latitude, longitude)} \cr
#' \item simply latitude \cr
#' \item a matrix (row 1-2 or col 1-2). The function will choose how to read the data \cr
#' \item a data.frame (col 1-2)
#' }
#' @param ... \itemize{
#' \item When \code{y} is only latitude, you can pass in \code{x} (longitude) here. \cr
#' \item when \code{y} is a vector of \code{c(lat, lon)}, you can pass in the rest vectors as well.
#' \item when \code{y} is a matrix or a data.frame, ... is omitted.
#' }
#'
#' @seealso \code{\link{convWGS2GCJ}}, \code{\link{convCoord}}.
#' @references
#' \url{https://on4wp7.codeplex.com/SourceControl/changeset/view/21483#353936}
#' @return A 2-col data.frame ([lng, lat]) of transformed coordinates.
#' @export
#'
#' @examples
#' \dontrun{
#' # Tiananmen square's GCJ-02 coordinate is c(39.908746, 116.397131)
#' # http://www.openstreetmap.org/#map=19/39.90734/116.39089
#'
#' ## Single point
#' convGCJ2WGS(c(39.908746, 116.397131))  # or
#' convGCJ2WGS(39.908746, 116.397131)  # get
#' #           lat      lng
#' # [1,] 39.90734 116.3909
#'
#' ## Multiple points
#' ### Vectors
#' convGCJ2WGS(c(39.908746, 116.397131), c(39.908746, 116.397131))  # get
#' #           lat      lng
#' # [1,] 39.90734 116.3909
#' # [2,] 39.90734 116.3909
#'
#' ### Matrix
#' m <- matrix(c(39.908746, 116.397131, 39.908746, 116.397131, 39.908746, 116.397131), nrow=2)
#' m
#' #           [,1]      [,2]      [,3]
#' # [1,]  39.90875  39.90875  39.90875
#' # [2,] 116.39713 116.39713 116.39713
#' convGCJ2WGS(m)  # get
#' #           lat      lng
#' # [1,] 39.90734 116.3909
#' # [2,] 39.90734 116.3909
#' # [3,] 39.90734 116.3909
#'
#' ### data.frame
#' df <- data.frame(lat=c(39.908746, 39.908746, 39.908746, NA),
#'                  lon=c(116.397131, 116.397131, 116.397131, 116.397131))
#' convGCJ2WGS(df)  # get
#' #           lat      lng
#' # [1,] 39.90734 116.3909
#' # [2,] 39.90734 116.3909
#' # [3,] 39.90734 116.3909
#' # [4,]       NA       NA
#' }
#'
convGCJ2WGS <- function(y, ...){
    # Coord  Mars GeoSystem (GCJ-02) -> Global GeoSystem (WGS-84)
    input <- getCoordArgs(y, ...)
    if (! all(abs(input['lat']) <= 90 & abs(input['lon'] <= 180), na.rm=TRUE)){
        stop(paste0("Latitude should be within [-90, 90] and longitude ",
                    "should be within [-180, 180]."))
    }
    output <- as.data.frame(t(apply(input, MARGIN=1, .gcj2wgs)))
    colnames(output) <- c("lat", "lng")
    return(output)
}

#' @rdname convGCJ2WGS
#' @export
gcj2wgs <- convGCJ2WGS

.gcj2wgs <- function(coord, ...){
    if (length(coord) > 1){
        gcLat <- coord[1]
        gcLon <- coord[2]
    }else{
        gcLat <- coord
        gcLon <- unlist(list(...))[1]
    }

    if (is.na(gcLat) || is.na(gcLon)){
        return(c(NA, NA))
    }else{
        if (! isOutOfChina(gcLat, gcLon)){
            Coords <- .wgs2gcj(c(gcLat, gcLon))
            dLat <- Coords[1]
            dLon <- Coords[2]
            GCJ2WGS <- c(2.0 * gcLat - dLat, 2.0 * gcLon - dLon)
        }else{
            GCJ2WGS <- c(gcLat, gcLon)
        }
    }
    return(GCJ2WGS)
}

#' Transform GCJ-02 (Google CN/Amap) coordinates to BD-09 (Baidu)
#'
#' GCJ-02 is coordinate system mandated by Chinese Gov't. BD-09 is Baidu specific
#' coordinates that encrypts GCJ-02 further more.
#' This function encrypts GCJ-02 into BD-09 coordinates (Baidu coordinates).
#'
#' @aliases gcj2bd
#' @note Latitude is the horizontal line serving as y-axis metric, longitude is
#' the vertical line serving as x-axis metric.
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param y \itemize{
#' \item A vector \code{c(latitude, longitude)} \cr
#' \item simply latitude \cr
#' \item a matrix (row 1-2 or col 1-2). The function will choose how to read the data \cr
#' \item a data.frame (col 1-2)
#' }
#' @param ... \itemize{
#' \item When \code{y} is only latitude, you can pass in \code{x} (longitude) here. \cr
#' \item when \code{y} is a vector of \code{c(lat, lon)}, you can pass in the rest vectors as well.
#' \item when \code{y} is a matrix or a data.frame, ... is omitted.
#' }
#'
#' @seealso \code{\link{convBD2GCJ}}, \code{\link{convCoord}}
#' @references
#' \url{https://on4wp7.codeplex.com/SourceControl/changeset/view/21483#353936}
#' @return A 2-col data.frame ([lng, lat]) of transformed coordinates.
#' @export
#'
#' @examples
#' \dontrun{
#' # Tiananmen square's GCJ-02 coordinate is c(39.908746, 116.397131)
#' #http://api.map.baidu.com/marker?location=39.91509,116.40350&title=Tiananmen&content=Tiananmen%20square&output=html
#'
#' ## Single point
#' convGCJ2BD(c(39.908746, 116.397131))  # or
#' convGCJ2BD(39.908746, 116.397131)  # get
#' #           lat      lng
#' # [1,] 39.91509 116.4035
#'
#' ## Multiple points
#' ### Vectors
#' convGCJ2BD(c(39.908746, 116.397131), c(39.908746, 116.397131))  # get
#' #           lat      lng
#' # [1,] 39.91509 116.4035
#' # [2,] 39.91509 116.4035
#'
#' ### Matrix
#' m <- matrix(c(39.908746, 116.397131, 39.908746, 116.397131, 39.908746, 116.397131), nrow=2)
#' m
#' #           [,1]      [,2]      [,3]
#' # [1,]  39.90875  39.90875  39.90875
#' # [2,] 116.39713 116.39713 116.39713
#' convGCJ2BD(m)  # get
#' #           lat      lng
#' # [1,] 39.91509 116.4035
#' # [2,] 39.91509 116.4035
#' # [3,] 39.91509 116.4035
#'
#' ### data.frame
#' df <- data.frame(lat=c(39.908746, 39.908746, 39.908746, NA),
#'                  lon=c(116.397131, 116.397131, 116.397131, 116.397131))
#' convGCJ2BD(df)  # get
#' #           lat      lng
#' # [1,] 39.91509 116.4035
#' # [2,] 39.91509 116.4035
#' # [3,] 39.91509 116.4035
#' # [4,]       NA       NA
#' }
#'
convGCJ2BD <- function(y, ...){
    # Coord Mars GeoSystem (GCJ-02) -> Baidu GeoSystem (BD-09)
    input <- getCoordArgs(y, ...)
    if (! all(abs(input['lat']) <= 90 & abs(input['lon'] <= 180), na.rm=TRUE)){
        stop(paste0("Latitude should be within [-90, 90] and longitude ",
                    "should be within [-180, 180]."))
    }
    output <- as.data.frame(t(apply(input, MARGIN=1, .gcj2bd)))
    colnames(output) <- c("lat", "lng")
    return(output)
}

#' @rdname convGCJ2BD
#' @export
gcj2bd <- convGCJ2BD

.gcj2bd <- function(coord, ...){
    if (length(coord) > 1){
        gcLat <- coord[1]
        gcLon <- coord[2]
    }else{
        gcLat <- coord
        gcLon <- unlist(list(...))[1]
    }

    if (is.na(gcLat) || is.na(gcLon)){
        return(c(NA, NA))
    }else{
        x <- gcLon
        y <- gcLat
        z <- sqrt(x ** 2 + y ** 2) + 0.00002 * sin(y * XM_PI)
        theta <- atn2(y, x) + 0.000003 * cos(x * XM_PI)
        GCJ2BD <- c(z * sin(theta) + 0.006, z * cos(theta) + 0.0065)
        return(GCJ2BD)
    }
}

#' Transform BD-09 (Baidu) coordinates to GCJ-02 (Google CN/Amap)
#'
#' GCJ-02 is coordinate system mandated by Chinese Gov't. BD-09 is Baidu specific
#' coordinates that encrypts GCJ-02 further more.
#' This function decrypts BD-09 back to GCJ-02 coordinates (Chinese coordinates).
#'
#' @aliases bd2gcj
#' @note Latitude is the horizontal line serving as y-axis metric, longitude is
#' the vertical line serving as x-axis metric.
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param y \itemize{
#' \item A vector \code{c(latitude, longitude)} \cr
#' \item simply latitude \cr
#' \item a matrix (row 1-2 or col 1-2). The function will choose how to read the data \cr
#' \item a data.frame (col 1-2)
#' }
#' @param ... \itemize{
#' \item When \code{y} is only latitude, you can pass in \code{x} (longitude) here. \cr
#' \item when \code{y} is a vector of \code{c(lat, lon)}, you can pass in the rest vectors as well.
#' \item when \code{y} is a matrix or a data.frame, ... is omitted.
#' }
#'
#' @seealso \code{\link{convBD2GCJ}}, \code{\link{convCoord}}.
#' @references
#' \url{https://on4wp7.codeplex.com/SourceControl/changeset/view/21483#353936}
#'
#' @return A 2-col data.frame ([lng, lat]) of transformed coordinates.
#' @export
#'
#' @examples
#' \dontrun{
#' # Tiananmen square's BD-06 coordinate is c(39.91509, 116.40350)
#' # http://www.google.cn/maps/place/Tiananmen,+Dongcheng,+Beijing/@39.90875,116.39713,16z?hl=en
#'
#' ## Single point
#' convBD2GCJ(c(39.91509, 116.40350))  # or
#' convBD2GCJ(39.91509, 116.40350)  # get
#' #           lat      lng
#' # [1,] 39.90875 116.3971
#'
#' ## Multiple points
#' ### Vectors
#' convBD2GCJ(c(39.91509, 116.40350), c(39.91509, 116.40350))  # get
#' #           lat      lng
#' # [1,] 39.90875 116.3971
#' # [2,] 39.90875 116.3971
#'
#' ### Matrix
#' m <- matrix(c(39.91509, 116.40350, 39.91509, 116.40350, 39.91509, 116.40350), nrow=2)
#' m
#' #           [,1]      [,2]      [,3]
#' # [1,]  39.91509  39.91509  39.91509
#' # [2,] 116.40350 116.40350 116.40350
#' convBD2GCJ(m)  # get
#' #           lat      lng
#' # [1,] 39.90875 116.3971
#' # [2,] 39.90875 116.3971
#' # [3,] 39.90875 116.3971
#'
#' ### data.frame
#' df <- data.frame(lat=c(39.91509, 39.91509, 39.91509, NA),
#'                  lon=c(116.40350, 116.40350, 116.40350, 116.40350))
#' convBD2GCJ(df)  # get
#' #           lat      lng
#' # [1,] 39.90875 116.3971
#' # [2,] 39.90875 116.3971
#' # [3,] 39.90875 116.3971
#' # [4,]       NA       NA
#' }
#'
convBD2GCJ <- function(y, ...){
    # Coord Baidu GeoSystem (BD-09) -> Mars GeoSystem (GCJ-02)
    input <- getCoordArgs(y, ...)
    if (! all(abs(input['lat']) <= 90 & abs(input['lon'] <= 180), na.rm=TRUE)){
        stop(paste0("Latitude should be within [-90, 90] and longitude ",
                    "should be within [-180, 180]."))
    }
    output <- as.data.frame(t(apply(input, MARGIN=1, .bd2gcj)))
    colnames(output) <- c("lat", "lng")
    return(output)
}

#' @rdname convBD2GCJ
#' @export
bd2gcj <- convBD2GCJ

.bd2gcj <- function(coord, ...){
    if (length(coord) >1){
        bdLat <- coord[1]
        bdLon <- coord[2]
    }else{
        bdLat <- coord
        bdLon <- unlist(list(...))[1]
    }
    if (is.na(bdLat) || is.na(bdLon)){
        return(c(NA, NA))
    }else{
        x <- bdLon - 0.0065
        y <- bdLat - 0.006
        z <- sqrt(x * x + y * y) - 0.00002 * sin(y * XM_PI)
        theta <- atn2(y, x) - 0.000003 * cos(x * XM_PI)
        BD2GCJ <- c(z * sin(theta), z * cos(theta))
        return(BD2GCJ)
    }
}

#' Transform WGS-84 (Global GeoSys) coordinates to BD-09 (Baidu)
#'
#' BD-09 is Baidu specific coordinates that encrypts GCJ-02 further more.
#' This function encrypts WGS-84 (Global) into BD-09 coordinates (Baidu coordinates).
#'
#' @aliases wgs2bd
#' @note Latitude is the horizontal line serving as y-axis metric, longitude is
#' the vertical line serving as x-axis metric.
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param y \itemize{
#' \item A vector \code{c(latitude, longitude)} \cr
#' \item simply latitude \cr
#' \item a matrix (row 1-2 or col 1-2). The function will choose how to read the data \cr
#' \item a data.frame (col 1-2)
#' }
#' @param ... \itemize{
#' \item When \code{y} is only latitude, you can pass in \code{x} (longitude) here. \cr
#' \item when \code{y} is a vector of \code{c(lat, lon)}, you can pass in the rest vectors as well.
#' \item when \code{y} is a matrix or a data.frame, ... is omitted.
#' }
#'
#' @seealso \code{\link{convBD2WGS}}, \code{\link{convWGS2GCJ}}, \code{\link{convCoord}}
#' @references
#' \url{https://on4wp7.codeplex.com/SourceControl/changeset/view/21483#353936}
#' @return A 2-col data.frame ([lng, lat]) of transformed coordinates.
#' @export
#'
#' @examples
#' \dontrun{
#' # Tiananmen square's WGS-84 coordinate is c(39.90734, 116.39089)
#' # http://api.map.baidu.com/marker?location=39.91509,116.40350&title=Tiananmen&content=Tiananmen%20square&output=html
#'
#' ## Single point
#' convWGS2BD(c(39.90734, 116.39089))  # or
#' convWGS2BD(39.90734, 116.39089)  # get
#' #           lat      lng
#' # [1,] 39.91508 116.4035
#'
#' ## Multiple points
#' ### Vectors
#' convWGS2BD(c(39.90734, 116.39089), c(39.90734, 116.39089))  # get
#' #           lat      lng
#' # [1,] 39.91508 116.4035
#' # [2,] 39.91508 116.4035
#'
#' ### Matrix
#' m <- matrix(c(39.90734, 116.39089, 39.90734, 116.39089, 39.90734, 116.39089), nrow=2)
#' m
#' #           [,1]      [,2]      [,3]
#' # [1,]  39.90734  39.90734  39.90734
#' # [2,] 116.39089 116.39089 116.39089
#' convWGS2BD(m)  # get
#' #           lat      lng
#' # [1,] 39.91508 116.4035
#' # [2,] 39.91508 116.4035
#' # [3,] 39.91508 116.4035
#'
#' ### data.frame
#' df <- data.frame(lat=c(39.90734, 39.90734, 39.90734, NA),
#'                  lon=c(116.39089, 116.39089, 116.39089, 116.39089))
#' convWGS2BD(df)  # get
#' #           lat      lng
#' # [1,] 39.91508 116.4035
#' # [2,] 39.91508 116.4035
#' # [3,] 39.91508 116.4035
#' # [4,]       NA       NA
#' }
#'
convWGS2BD <- function(y, ...){
    # Global coord (WGS-84) -> Baidu GeoSystem (BD-09)
    intermediate <- convWGS2GCJ(y, ...)
    return(convGCJ2BD(intermediate))
}

#' @rdname convWGS2BD
#' @export
wgs2bd <- convWGS2BD

#' Transform BD-09 (Baidu) coordinates to WGS-84 (Global Coord)
#'
#' BD-09 is Baidu specific coordinates that encrypts GCJ-02 further more.
#' This function decrypts BD-09 back to WGS-84 coordinates (Global coordinates).
#'
#' @aliases bd2wgs
#' @note Latitude is the horizontal line serving as y-axis metric, longitude is
#' the vertical line serving as x-axis metric.
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param y \itemize{
#' \item A vector \code{c(latitude, longitude)} \cr
#' \item simply latitude \cr
#' \item a matrix (row 1-2 or col 1-2). The function will choose how to read the data \cr
#' \item a data.frame (col 1-2)
#' }
#' @param ... \itemize{
#' \item When \code{y} is only latitude, you can pass in \code{x} (longitude) here. \cr
#' \item when \code{y} is a vector of \code{c(lat, lon)}, you can pass in the rest vectors as well.
#' \item when \code{y} is a matrix or a data.frame, ... is omitted.
#' }
#'
#' @seealso \code{\link{convWGS2BD}}, \code{\link{convBD2GCJ}}, \code{\link{convCoord}}.
#' @references
#' \url{https://on4wp7.codeplex.com/SourceControl/changeset/view/21483#353936}
#'
#' @return A 2-col data.frame ([lng, lat]) of transformed coordinates.
#' @export
#'
#' @examples
#' \dontrun{
#' # Tiananmen square's BD-06 coordinate is c(39.91509, 116.40350)
#' # http://www.openstreetmap.org/#map=19/39.90734/116.39089
#'
#' ## Single point
#' convBD2WGS(c(39.91509, 116.40350))  # or
#' convBD2WGS(39.91509, 116.40350)  # get
#' #           lat      lng
#' # [1,] 39.90734 116.3909
#'
#' ## Multiple points
#' ### Vectors
#' convBD2WGS(c(39.91509, 116.40350), c(39.91509, 116.40350))  # get
#' #           lat      lng
#' # [1,] 39.90734 116.3909
#' # [2,] 39.90734 116.3909
#'
#' ### Matrix
#' m <- matrix(c(39.91509, 116.40350, 39.91509, 116.40350, 39.91509, 116.40350), nrow=2)
#' m
#' #           [,1]      [,2]      [,3]
#' # [1,]  39.90734  39.90734  39.90734
#' # [2,] 116.39089 116.39089 116.39089
#' convBD2WGS(m)  # get
#' #           lat      lng
#' # [1,] 39.90734 116.3909
#' # [2,] 39.90734 116.3909
#' # [3,] 39.90734 116.3909
#'
#' ### data.frame
#' df <- data.frame(lat=c(39.91509, 39.91509, 39.91509, NA),
#'                  lon=c(116.40350, 116.40350, 116.40350, 116.40350))
#' convBD2WGS(df)  # get
#' #           lat      lng
#' # [1,] 39.90734 116.3909
#' # [2,] 39.90734 116.3909
#' # [3,] 39.90734 116.3909
#' # [4,]       NA       NA
#' }
#'
convBD2WGS <- function(y, ...){
    # Coord Baidu GeoSystem (BD-09) -> Global GeoSystem (WGS-84)
    intermediate <- convBD2GCJ(y, ...)
    return(convGCJ2WGS(intermediate))
}

#' @rdname convBD2WGS
#' @export
bd2wgs <- convBD2WGS

#' Generic Function to Convert coordinates
#'
#' the general function that converts lat/lon coordintes from one GCS to another
#' GCS including WGS-84, GCJ-02 and BD-09 either locally or by calling Baidu
#' Maps API.
#'
#' @aliases ecoord
#' @param lat a numeric latitude
#' @param lon a numeric longitude
#' @param from the inputting GCS
#' @param to the outputting GCS
#' @param api use baidu maps api. Note that baidu maps api only supports the
#' transformations from WGS-84 or GCJ-02 to BD-09. Other coodinate conversions
#' must be done locally. As the conversion result is the same, it's recommended
#' to perform conversions locally.
#' @return a 2-col data.frame ([lng, lat]) of transformed coordinates.
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD student from
#' Center for Earth System Science, Tsinghua University
#' @details Note that the baidu maps api limits to 20 lat/lon coordinates per query.
#' Since the coordinate conversion results of Baidu Maps API and local algorithms
#' are the same, it is recommended to use local algorithms.
#' @seealso \code{\link{convWGS2GCJ}}, \code{\link{convWGS2BD}}, \code{\link{convGCJ2WGS}},
#' \code{\link{convGCJ2BD}}, \code{\link{convBD2WGS}}, \code{\link{convBD2GCJ}}.
#' @export
#' @import curl jsonlite
#' @examples
#' \dontrun{
#' # latitude/longitude coordinates of Beijing railway station
#' # WGS-84: (39.90105, 116.42079)
#' # GCJ-02: (39.90245, 116.42703)
#' # BD-09:  (39.90851, 116.43351)
#' convCoord(39.90105, 116.42079, from = 'WGS-84', to = 'GCJ-02')
#' convCoord(39.90105, 116.42079, from = 'WGS-84', to = 'GCJ-02', api = TRUE)
#' convCoord(39.90105, 116.42079, from = 'WGS-84', to = 'BD-09')
#' convCoord(39.90105, 116.42079, from = 'WGS-84', to = 'BD-09', api = TRUE)
#' convCoord(39.90245, 116.42703, from = 'GCJ-02', to = 'WGS-84')
#' # not supported by baidu map api, return NAs
#' convCoord(39.90245, 116.42703, from = 'GCJ-02', to = 'WGS-84', api = TRUE)
#' convCoord(39.90245, 116.42703, from = 'GCJ-02', to = 'BD-09')
#' convCoord(39.90245, 116.42703, from = 'GCJ-02', to = 'BD-09', api = TRUE)
#' convCoord(39.90851, 116.43351, from = 'BD-09', to = 'GCJ-02')
#' # not supported by baidu map api, return NAs
#' convCoord(39.90851, 116.43351, from = 'BD-09', to = 'GCJ-02', api = TRUE)
#' convCoord(39.90851, 116.43351, from = 'BD-09', to = 'WGS-84')
#' # not supported by baidu map api, return NAs
#' convCoord(39.90851, 116.43351, from = 'BD-09', to = 'WGS-84', api = TRUE)
#' # convert multiple coordinates
#' lat = c(39.99837, 39.98565)
#' lng = c(116.3203, 116.2998)
#' convCoord(lat, lng, from = 'WGS-84', to = 'GCJ-02')
#' }
convCoord <- function(lat, lon, from = c('WGS-84', 'GCJ-02', 'BD-09'),
                 to = c('WGS-84', 'GCJ-02', 'BD-09'), api = FALSE){
    # check parameters
    stopifnot(is.numeric(lat))
    stopifnot(is.numeric(lon))
    from <- match.arg(from)
    to <- match.arg(to)
    stopifnot(is.logical(api))

    # vectorize
    if(length(lat) > 1){
        return(ldply(seq_along(lat), function(i){
            convCoord(lat[i], lon[i], from = from, to = to, api = api) }))
    }

    if(from == to){
        return(data.frame(lat = lat, lng = lon))
    } else{
        if(api){
            # coordinate system lookup table
            code <- c(0, 2, 4)
            names(code) <- c('WGS-84', 'GCJ-02', 'BD-09')
            f <- code[from]
            t <- code[to]

            # format url
            # http://api.map.baidu.com/ag/coord/convert?x=lon&y=lat&from=FROM&to=TO
            url_string <- paste('http://api.map.baidu.com/ag/coord/convert?x=', lon,
                                '&y=', lat, '&from=', f, '&to=', t, sep = '')
            message(paste('calling ', url_string, ' ... ', sep = ''), appendLF = F)

            # convert
            con <- curl(URLencode(url_string))
            cv <- jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = ''), drop = FALSE)
            message('done.')
            close(con)

            # did convert fail?
            if(is.list(cv)){
                if(cv$error == 0){
                    cvdf <- with(cv, {data.frame(lat = as.numeric(base64(y, FALSE)),
                                                 lng = as.numeric(base64(x, FALSE)),
                                                 row.names = NULL)})
                    return(cvdf)
                }
            } else{
                warning(paste('convert failed with error ', cv['error'],
                              '. conversion is not supported by baidu map api.',
                              sep = ''),
                        call. = FALSE)
                return(data.frame(lat = NA, lng = NA))
            }
        } else{
            if(from == 'WGS-84' & to == 'GCJ-02') return(convWGS2GCJ(lat, lon))
            if(from == 'WGS-84' & to == 'BD-09') return(convWGS2BD(lat, lon))
            if(from == 'GCJ-02' & to == 'WGS-84') return(convGCJ2WGS(lat, lon))
            if(from == 'GCJ-02' & to == 'BD-09') return(convGCJ2BD(lat, lon))
            if(from == 'BD-09' & to == 'WGS-84') return(convBD2WGS(lat, lon))
            if(from == 'BD-09' & to == 'GCJ-02') return(convBD2GCJ(lat, lon))
        }
    }
}

#' @rdname convCoord
#' @export
ecoord <- convCoord

#' Geocode
#'
#' geocodes an address using Google or Baidu Maps API. Note that in most cases by
#' using this function you are agreeing to the Google Maps API Terms of Service
#' at \url{https://developers.google.com/maps/terms} or the Baidu Maps API Terms
#' of Use at \url{http://developer.baidu.com/map/law.htm}.
#'
#' @param address a character vector specifying a location of interest (e.g.,
#' "Tsinghua Univeristy").
#' @param api use Google or Baidu Maps API. When using Baidu Maps API, the address must be in Chinese.
#' @param key an api key must be provided when calling baidu maps api.
#' While it's unnecessary for calling google maps api.
#' @param ocs output coordinate systems including WGS-84, GCJ-02 and BD-09, which
#' are the GCSs of Google Earth, Google Map in China and Baidu Map, respectively.
#' For address out of China, ocs is automatically set to 'WGS-84' and other values
#' are igored.
#' @param output lat/lng coordinates or lat/lng coordinates with location type (Goolge Map) | confidence
#' (Baidu Map) or lat/lng coordinates with formated address and address components (only available for #' Google Map API).
#' @param messaging turn messaging on/off. The default value is FALSE.
#' @param time the time interval to geocode, in seconds. Default value is zero.
#' When you geocode multiple addresses, set a proper time interval to avoid
#' exceeding usage limits. For details see
#' \url{https://developers.google.com/maps/documentation/business/articles/usage_limits}
#' @return a data.frame with variables lat/lng or lat/lng/conf
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD student from
#' Center for Earth System Science, Tsinghua University
#' @details note that the google maps api limits to 2500 queries a day.
#' @seealso \code{\link{revgeocode}}, \code{\link{geohost}}.
#'
#' Google Maps API at \url{http://code.google.com/apis/maps/documentation/geocoding/}
#' and Baidu Maps API at \url{http://developer.baidu.com/map/webservice-geocoding.htm}
#' @export
#' @importFrom plyr ldply llply
#' @examples
#' \dontrun{
#' geocode('Tsinghua University', api = 'google', ocs = 'GCJ-02')
#' geocode('Tsinghua University', api = 'google', ocs = 'WGS-84',
#'         messaging = TRUE)
#' geocode('Beijing railway station', api = 'google', ocs = 'WGS-84',
#'         output = 'latlngc')
#' geocode('Beijing railway station', api = 'google', ocs = 'WGS-84',
#'         output = 'latlnga')
#' geocode(c('Tsinghua University', 'Beijing railway station'), api = 'google',
#'         ocs = 'GCJ-02')
#' geocode(c('Tsinghua University', 'Beijing railway station'), api = 'google',
#'         ocs = 'WGS-84', output = 'latlngc', messaging = TRUE)
#' geocode(c('Tsinghua University', 'Beijing railway station'), api = 'google',
#'         ocs = 'WGS-84', output = 'latlnga', messaging = TRUE)
#' geocode(c('Tsinghua University', 'Beijing railway station'), api = 'google',
#'         ocs = 'WGS-84', output = 'latlngc', messaging = TRUE, time = 2)
#' geocode('Beijing railway station', api = 'baidu', key = 'your baidu maps api key',
#' ocs = 'BD-09')
#' geocode('Beijing railway station', api = 'baidu', key = 'your baidu maps api key',
#' ocs = 'GCJ-02', messaging = TRUE)
#' geocode('Beijing railway station', api = 'baidu', key = 'your baidu maps api key',
#' ocs = 'BD-09', output = 'latlngc')
#' geocode(c('Tsinghua University', 'Beijing railway station'), api = 'baidu',
#' key = 'your baidu maps api key', ocs = 'BD-09')
#' geocode(c('Tsinghua University', 'Beijing railway station'), api = 'baidu',
#' key = 'your baidu maps api key', ocs = 'WGS-84', output = 'latlngc')
#' }
geocode <- function(address, api = c('google', 'baidu'), key = '',
                    ocs = c('WGS-84', 'GCJ-02', 'BD-09'),
                    output = c('latlng', 'latlngc', 'latlnga'), messaging = FALSE,
                    time = 0){
    # check parameters
    stopifnot(is.character(address))
    api <- tolower(match.arg(api))
    stopifnot(is.character(key))
    output <- tolower(match.arg(output))
    ocs <- toupper(match.arg(ocs))
    stopifnot(is.logical(messaging))
    stopifnot(is.numeric(time))

    # vectorize for many addresses
    if (length(address) > 1) {
        if (api == 'google') {
            s <- 'google restricts requests to 2500 requests a day.'
            if (length(address) > 2500) stop(s, call. = F)
            if (length(address) > 200 & messaging) message(paste('Reminder', s, sep = ' : '))
        }

        return(ldply(address, function(add){
            Sys.sleep(time)
            geocode(add, api = api, key = key, ocs = ocs, output = output, messaging = messaging)
        }))
    }

    # location encoding
    address <- enc2utf8(address)
    # different google maps api is used based user's location. If user is inside China,
    # ditu.google.cn is used; otherwise maps.google.com is used.
    if (api == 'google') {
        cname <- try(ip.country(), TRUE)
        if (cname == "CN") {
            api_url <- 'http://ditu.google.cn/maps/api/geocode/json'
        } else{
            api_url <- 'http://maps.googleapis.com/maps/api/geocode/json'
        }
    } else{
        api_url <- 'http://api.map.baidu.com/geocoder/v2/'
    }
    # format url
    # https is only supported on Windows, when R is started with the --internet2
    # command line option. without this option, or on Mac, you will get the error
    # "unsupported URL scheme".
    if (api == 'google') {
        # http://maps.googleapis.com/maps/api/geocode/json?address=ADDRESS&sensor
        # =false&key=API_KEY for outside China
        # http://ditu.google.cn/maps/api/geocode/json?address=ADDRESS&sensor
        # =false&key=API_KEY for outside China
        url_string <- paste(api_url, '?address=', address, '&sensor=false', sep = '')
        if (nchar(key) > 0) {
            url_string <- paste(url_string, '&key=', key, sep = '')
        }
    }
    if (api == 'baidu') {
        # http://api.map.baidu.com/geocoder/v2/?address=ADDRESS&output=json&ak=API_KEY
        url_string <- paste(api_url, '?address=', address, '&output=json&ak=', key, sep = '')
    }

    if (messaging) message(paste('calling ', url_string, ' ... ', sep = ''),
                           appendLF = FALSE)

    # geocode
    # con <- curl(URLencode(url_string))
    con <- URLencode(url_string)
    gc <- fromJSON(paste(readLines(con, warn = FALSE), collapse = ''))
    if (messaging) message('done.')
    #close(con)

    # geocoding results
    if (api == 'google') {
        # did geocode fail?
        if (gc$status != 'OK') {
            warning(paste('geocode failed with status ', gc$status, ', location = "',
                          address, '"', sep = ''), call. = FALSE)
            return(data.frame(lat = NA, lng = NA))
        }

        # more than one location found?
        if (length(gc$results) > 1 && messaging) {
            Encoding(gc$results[[1]]$formatted_address) <- "UTF-8"
            message(paste('more than one location found for "', address,
                          '", using address\n"',
                          tolower(gc$results[[1]]$formatted_address),
                          '"', sep = ''), appendLF = TRUE)
        }

        gcdf <- with(gc$results, {
            data.frame(lat = NULLtoNA(geometry$location['lat']),
                       lng = NULLtoNA(geometry$location['lng']),
                       loctype = tolower(NULLtoNA(geometry$location_type)),
                       address = tolower(NULLtoNA(formatted_address)),
                       row.names = NULL)})

        # address components
        # attrdf <- ldply(gc$results$address_components, function(l){
        #     l <- lapply(l, function(li) {
        #         if (length(li) == 0) li <- NA else li
        #     })
        #     as.data.frame(l, stringsAsFactors = FALSE)[1, ]
        # })
        attrdf <- gc$results$address_components[[1]]
        attrdf <- attrdf[!is.na(attrdf$types), c('types', 'long_name')]
        gcdf <- within(gcdf, {
            poi <-
                tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'point_of_interest']))
            street_no <-
                as.numeric(NULLtoNA(attrdf$long_name[attrdf$types == 'street_number']))
            route <-
                tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'route']))
            subloc_l3 <-
                tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'sublocality_level_3'])) # village
            subloc_l2 <-
                tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'sublocality_level_2'])) # town
            subloc_l1 <-
                tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'sublocality_level_1'])) # distrcit/county
            locality <-
                tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'locality'])) # city
            admin_area_l2 <-
                tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'administrative_area_level_2'])) # US county
            admin_area_l1 <-
                tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'administrative_area_level_1'])) # prvince or US state
            country <-
                tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'country']))
            postal_code <-
                tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'postal_code']))
        })

        # convert coordinates only in China
        if (!isOutOfChina(gcdf[, 'lat'], gcdf[, 'lng'])) {
            gcdf[c('lat', 'lng')] <- convCoord(gcdf[, 'lat'], gcdf[, 'lng'], from = 'GCJ-02', to = ocs)
        } else{
            if (ocs != 'WGS-84') {
                message('wrong usage: for address out of China, ocs can only be set to "WGS-84"',
                        appendLF = TRUE)
                ocs <- 'WGS-84'
            }
        }

        if (output == 'latlng') return(gcdf[c('lat', 'lng')])
        if (output == 'latlngc') return(gcdf[c('lat', 'lng', 'loctype')])
        if (output == 'latlnga') return(gcdf[!names(gcdf) %in% c('loctype')])
    }
    if (api == 'baidu') {
        # did geocode fail?
        if (gc$status != 0) {
            warning(paste('geocode failed with status code ', gc$status, ', location = "',
                          address, '". see more details in the response code table of Baidu Geocoding API',
                          sep = ''), call. = FALSE)
            return(data.frame(lat = NA, lng = NA))
        }

        gcdf <- with(gc$result, {data.frame(lat = NULLtoNA(location['lat']),
                                            lng = NULLtoNA(location['lng']),
                                            conf = NULLtoNA(confidence),
                                            row.names = NULL)})

        # convert coordinates
        gcdf[c('lat', 'lng')] <- convCoord(gcdf[, 'lat'], gcdf[, 'lng'], from = 'BD-09', to = ocs)

        if (output == 'latlnga') {
            message('Baidu map geocoder cannot return address, please try Goolge map geocoder.',
                    appendLF = TRUE)
            output <- 'latlng'
        }
        if (output == 'latlng') return(gcdf[c('lat', 'lng')])
        if (output == 'latlngc') return(gcdf[c('lat', 'lng', 'conf')])
    }
}

# fill NULL with NA
NULLtoNA <- function(x){
    if (is.null(x)) return(NA)
    if (is.character(x) & length(x) == 0) return(NA)
    x
}

#' IP address lookup
#'
#' geocodes an IP address using either freegeoip.net \url{http://freegeoip.net} or
#' ipinfo.io \url{http://ipinfo.io/developers} IP lookup API.
#'
#' @section GIS:
#' @param ip a character vector specifying an IP (e.g., "12.215.42.19").
#' The default value is no IP is specified and the host IP is used.
#' @param api use freegeoip.net or ipinfo.io IP lookup API. By default
#' freegeoip.net is used.
#' @return a vector with information of ip, country_code, country_name, region_code,
#' city, zip_code, time_zone, latitude, longitude and metro_code for freegeoip.net API,
#' of ip, hostname, city, region, country, loc, org, postal and phone for ipinfo.io
#' IP lookup API. If numerous IPs are inputted, a data.frame is returned.
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD student from
#' Center for Earth System Science, Tsinghua University
#' @details note that freegeoip.net API is allowed up to 10,000 queries per hour
#' by default, ipinfo API is limited to 1,000 requests per day.
#' @seealso \code{\link{geocode}}, \code{\link{revgeocode}}.
#' @references
#' freegeoip.net IP lookup API at \url{http://freegeoip.net}
#' ipinfo.io IP lookup API at \url{http://ipinfo.io/developers}
#' @export
#' @examples
#' \dontrun{
#' # geocode host IP
#' geohost()
#' geohost(api = "ipinfo.io")
#' # specify an IP for geocoding
#' geohost(ip = "12.215.42.19")
#' geohost(ip = "12.215.42.19", api = "ipinfo.io")
#' # geocode multiple IPs
#' geohost(ip = c("61.135.169.81", "12.215.42.19"))
#' geohost(ip = c("61.135.169.81", "12.215.42.19"), api = "ipinfo.io")
#' }
geohost <- function(ip = '', api = c("freegeoip.net", "ipinfo.io")) {
    # check parameters
    stopifnot(is.character(ip))
    api <- match.arg(api)

    # vectorize for many IPs
    if (length(ip) > 1) {
        return(ldply(ip, geohost, api = api))
    }

    if (api == "freegeoip.net") {
        url_string <- "http://freegeoip.net/json/"
        if (nchar(ip) > 0) {
            url_string <- paste0(url_string, ip)
        }
        con <- curl(url_string)
        x <- readLines(con, warn = FALSE)
        close(con)
        as.data.frame(fromJSON(x))
    } else {
        url_string <- "http://ipinfo.io/json"
        if (nchar(ip) > 0) {
            url_string <- paste0("http://ipinfo.io/", ip, "/json")
        }
        con <- curl(URLencode(url_string))
        x <- paste(readLines(con, warn = F), collapse = "")
        close(con)
        as.data.frame(as.list(fromJSON(x)))
    }
}

# option ip.country to store IP country and avoid calling geohost() repeatedly
# when geocoding multiple addresses or revgeocoding multiple locations
ip.country <- function(){
    if (!"ip.country" %in% names(options())) {
        options(ip.country = geohost(api = "ipinfo.io")["country"])
    }
    getOption("ip.country")
}

#' Reverse geocode
#'
#' reverse geocodes a lat/lng location using Google or Baidu Maps API.  Note that in
#' most cases by using this function you are agreeing to the Google Maps API Terms
#' of Service at \url{https://developers.google.com/maps/terms} or the Baidu Maps
#' API Terms of Use at \url{http://developer.baidu.com/map/law.htm}.
#'
#' @param latlng a location in latitude/longitude format
#' @param ics the coordinate system of inputing location, including WGS-84, GCJ-02
#' and BD-09, which are the GCSs of Google Earth, Google Map in China and Baidu
#' Map, respectively. For location out of China, ics is automatically set to 'WGS-84'
#' and other values are ignored.
#' @param api use google or baidu maps api
#' @param key an api key must be provided when calling baidu maps api.
#' While it's unnecessary for calling google maps api.
#' @param output formatted address or formmatted address with address components
#' @param messaging turn messaging on/off. The default value is FALSE.
#' @param time the time interval to revgeocode, in seconds. Default value is zero.
#' When you revgeocode multiple locations, set a proper time interval to avoid
#' exceeding usage limits. For details see
#' \url{https://developers.google.com/maps/documentation/business/articles/usage_limits}
#' @return a data.frame with variables address or detail address components
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD student from
#' Center for Earth System Science, Tsinghua University
#' @details note that the google maps api limits to 2500 queries a day.
#' @seealso \code{\link{geocode}}, \code{\link{geohost}}.
#'
#' Google Maps API at \url{http://code.google.com/apis/maps/documentation/geocoding/}
#' and Baidu Maps API at \url{http://developer.baidu.com/map/webservice-geocoding.htm}
#' @export
#' @examples
#' \dontrun{
#' # reverse geocode Beijing railway station
#' revgeocode(c(39.90105, 116.42079), ics = 'WGS-84', api = 'google',
#'            output = 'address')
#' revgeocode(c(39.90245, 116.42703), ics = 'GCJ-02', api = 'google',
#'            output = 'address', messaging = TRUE)
#' revgeocode(c(39.90851, 116.43351), ics = 'BD-09', api = 'google',
#'            output = 'addressc')
#' revgeocode(c(39.90851, 116.43351), ics = 'BD-09', api = 'baidu',
#'            key = 'your baidu maps api key', output = 'address')
#' revgeocode(c(39.90245, 116.42703), ics = 'GCJ-02', api = 'baidu',
#'            key = 'your baidu maps api key', output = 'address', messaging = TRUE)
#' revgeocode(c(39.90105, 116.42079), ics = 'WGS-84', api = 'baidu',
#'            key = 'your baidu maps api key', output = 'addressc')
#' # reverse geocode multiple locations
#' latlng = data.frame(lat = c(39.99837, 39.98565), lng = c(116.3203, 116.2998))
#' revgeocode(latlng, ics = 'WGS-84', api = 'google', output = 'address')
#' revgeocode(latlng, ics = 'WGS-84', api = 'google', output = 'address', time = 2)
#' }
revgeocode <- function(latlng, ics = c('WGS-84', 'GCJ-02', 'BD-09'),
                       api = c('google', 'baidu'), key = '',
                       output = c('address', 'addressc'), messaging = FALSE,
                       time = 0){
    # check parameters
    stopifnot(class(latlng) %in% c('numeric', 'data.frame'))
    ics <- toupper(match.arg(ics))
    api <- toupper(match.arg(api))
    stopifnot(is.character(key))
    output <- match.arg(output)
    stopifnot(is.logical(messaging))
    stopifnot(is.numeric(time))

    # vectorize for many locations
    if(is.data.frame(latlng)){
        return(ldply(seq_along(latlng), function(i){
            Sys.sleep(time)
            revgeocode(as.numeric(latlng[i, ]), ics = ics, api = api, key = key,
                       output = output, messaging = messaging) }))
    }

    # different google maps api is used based user's location. If user is inside China,
    # ditu.google.cn is used; otherwise maps.google.com is used.
    if(api == 'google'){
        cname <- try(ip.country(), TRUE)
        if(cname == "CN"){
            api_url <- 'http://ditu.google.cn/maps/api/geocode/json'
        } else{
            api_url <- 'http://maps.googleapis.com/maps/api/geocode/json'
        }
    } else{
        api_url <- 'http://api.map.baidu.com/geocoder/v2/'
    }

    # format url
    if(api == 'google'){
        # convert coordinates only in China
        if(!isOutOfChina(latlng[1], latlng[2])){
            latlng <- convCoord(latlng[1], latlng[2], from = ics, to = 'GCJ-02')
        } else{
            if(ics != 'WGS-84'){
                message('wrong usage: for location out of China, ics can only be set to "WGS-84"',
                        appendLF = TRUE)
            }
        }

        # http://maps.googleapis.com/maps/api/geocode/json?latlng=LAT,LNG
        # &sensor=FALSE&key=API_KEY for outside China
        # http://ditu.google.com/maps/api/geocode/json?latlng=LAT,LNG
        # &sensor=FALSE&key=API_KEY for inside China
        url_string <- paste(api_url, '?latlng=', latlng[1], ',', latlng[2],
                            '&sensor=false', sep = '')
        if(nchar(key) > 0){
            url_string <- paste(url_string, '&key=', key, sep = '')
        }
    }
    if(api == 'baidu'){
        # coordinate type lookup table
        code <- c('wgs84ll', 'gcj02ll', 'bd09ll')
        names(code) <- c('WGS-84', 'GCJ-02', 'BD-09')
        coordtype <- code[ics]
        # http://api.map.baidu.com/geocoder/v2/?location=LAT,LNG&coordtype=COORDTYPE
        # &output=json&ak=API_KEY
        url_string <- paste(api_url, '?location=', latlng[1], ',', latlng[2],
                            '&coordtype=', coordtype, '&output=json&ak=', key, sep = '')
    }

    if(messaging) message(paste('calling ', url_string, ' ... ', sep = ''),
                          appendLF = FALSE)

    # reverse gecode
    con <- curl(URLencode(url_string))
    rgc <- fromJSON(paste(readLines(con, warn = FALSE), collapse = ''))
    if(messaging) message('done.')
    close(con)

    # reverse geocoding results
    if(api == 'google'){
        # did reverse geocoding fail?
        if(rgc$status != 'OK'){
            warning(paste('reverse geocode failed with status ', gc$status, ',
                          location = "', latlng[1], ', ', latlng[2], '"', sep = ''),
                    call. = FALSE)
            return(data.frame(address = NA))
        }

        # more than one address found?
        if(length(rgc$results) > 1 && messaging){
            message(paste('more than one address found for "', latlng[1], ', ',
                          latlng[2],  '", reverse geocoding first ... ', sep = ''),
                    apppendLF = TRUE)
        }

        rgcdf <- with(rgc$results[[1]], {data.frame(address = formatted_address,
                                                    row.names = NULL)})
        for(i in seq_along(rgc$results[[1]]$address_components)){
            rgcdf <- cbind(rgcdf, rgc$results[[1]]$address_components[[i]]$long_name)
        }
        names(rgcdf) <- c('address', sapply(rgc$results[[1]]$address_components,
                                            function(l) l$types[1]))
    }
    if(api == 'baidu'){
        # did geocode fail?
        if(rgc$status != 0){
            warning(paste('geocode failed with status code ', rgc$status, ',
                          location = "', latlng[1], ', ', latlng[2],
                          '". see more details in the response code table of Baidu Geocoding API',
                          sep = ''), call. = FALSE)
            return(data.frame(address = NA))
        }

        rgcdf <- with(rgc$result, {
            data.frame(address = formatted_address,
                       street_number = NULLtoNA(addressComponent['street_number']),
                       street = NULLtoNA(addressComponent['street']),
                       district = NULLtoNA(addressComponent['district']),
                       city = NULLtoNA(addressComponent['city']),
                       province = NULLtoNA(addressComponent['province']),
                       row.names = NULL)})
    }

    if(output == 'address') return(rgcdf['address'])
    if(output == 'addressc') return(rgcdf)
}



