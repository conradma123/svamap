##' rt90_sweref99
##'
##' Change all projected coordinated from sweref99 to RT90. The
##' coordinate systems do not overlap. All those points with a
##' Y-coordinate (the horizontal axis) of less than 1083427.290 are
##' sweref99 and should be reprojected to RT90. The other points are
##' already in RT90. The reason for the choice of RT90 is that the
##' polygon data that I have for Sweden is in RT90.
##'
##' This function is designed to work on the PPN dataset from
##' rapportportalen (RP).
##'
##' @title rt90_sweref99
##' @param df a dataframe (read from the PPN dataset in RP)
##' @return a dataframe
##' @author Thomas Rosendal
##' @export
rt90_sweref99 <- function(df) {
    stopifnot(all(c("X", "Y") %in% names(df)))
    ## Rename the columns appropriatly since RP has inverted XY nomenclature
    colnames(df)[colnames(df) == "Y"] <- "X.1"
    colnames(df)[colnames(df) == "X"] <- "Y"
    colnames(df)[colnames(df) == "X.1"] <- "X"
    ## First check if there is anything to be done. Perhaps all of the points are in RT90.
    if (length(df$Y[df$Y < 1083427.2970 & !is.na(df$Y) & !is.na(df$X)]) == 0) {return(df); break}
    ## A subset that contains just the points that are sweref99
    points_sweref99 <- SpatialPoints(cbind(df$Y[df$Y < 1083427.2970 & !is.na(df$Y) & !is.na(df$X)],
                                           df$X[df$Y < 1083427.2970 & !is.na(df$Y) & !is.na(df$X)]))
    ## Assign the projection to the object
    proj4string(points_sweref99) <- "+init=epsg:3006 +proj=utm +zone=33 +ellps=GRS80
  +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
    ## A subset that is just the RT90 points
    points_RT90 <- SpatialPoints(cbind(df$Y[df$Y >= 1083427.2970 & !is.na(df$Y) & !is.na(df$X)],
                                       df$X[df$Y >= 1083427.2970 & !is.na(df$Y) & !is.na(df$X)]))
    ## Assign the project to that object
    proj4string(points_RT90) <- "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0
                                 +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0 +units=m +no_defs"
    ## Project the sweref99 points to RT90
    points_RT90_2<- spTransform(points_sweref99, "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0
                                                  +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0 +units=m +no_defs")
    ## Assign the coordinates back to the orignal dataframe
    df$X1[df$Y<1083427.2970 & !is.na(df$Y) & !is.na(df$X)] <- points_RT90_2@coords[,"coords.x2"]
    df$Y1[df$Y<1083427.2970 & !is.na(df$Y) & !is.na(df$X)] <- points_RT90_2@coords[,"coords.x1"]
    df$X2[df$Y>=1083427.2970 & !is.na(df$Y) & !is.na(df$X)] <- points_RT90@coords[,"coords.x2"]
    df$Y2[df$Y>=1083427.2970 & !is.na(df$Y) & !is.na(df$X)] <- points_RT90@coords[,"coords.x1"]
    df$X <- df$X1
    df$X[is.na(df$X)] <- df$X2[is.na(df$X)]
    df$Y <- df$Y1
    df$Y[is.na(df$Y)] <- df$Y2[is.na(df$Y)]
    ## Trash the extra columns
    df <- df[,!(names(df) %in% c("X1", "Y1", "X2", "Y2"))]
    ## Invert the names again
    colnames(df)[colnames(df) == "Y"] <- "X.1"
    colnames(df)[colnames(df) == "X"] <- "Y"
    colnames(df)[colnames(df) == "X.1"] <- "X"
    return(df)
}
