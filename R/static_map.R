##' Create a static map
##'
##' @title point_map
##' 
##' FYI the following are some proj4string specs. RT90 is the default
##'
##' RT90 is
##'
##' "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0 +units=m +no_defs"
##'
##' SWEREF99 is
##'
##' "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
##'
##' WGS84 is
##'
##' "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
##' @param pts a spatialopointsdataframe
##' @param by an optional argument indicating the value to use to
##'     assign colours. A factor.
##' @param map_proj The projection of those output map (represented in
##'     proj4string)
##' @param basemap The name of the basemap
##' @param path_to_file The output file
##' @param width The width in inches
##' @param height The height in inches
##' @param pch The plotting character
##' @param col The col of the plotting character. If 'by' is specified
##'     then it should be a vector with length == levels(by)
##' @param cex The size of the plotting character
##' @param border_col the colour of the polygon borders
##' @param border_lwd The thicknes of the polygon borders
##' @param legend Want a legend ? TRUE or FALSE
##' @param legend_labs A vector of the legend labels. Default is the
##'     levels of the by variable. There must be a by variable to get
##'     a legend
##' @param legend_position The x y position of the legend in the same
##'     coordinate system as the map
##' @return A path to the pdf file of the map
##' @author Thomas Rosendal
##' @import utils
##' @import sp
##' @import rgdal
##' @import stats
##' @import grDevices
##' @export
point_map <- function(pts,
                      by = NULL,
                      map_proj = "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0 +units=m +no_defs",
                      basemap = "NUTS_20M",
                      path_to_file = tempfile(fileext = ".pdf"),
                      height = 7,
                      width = 5,
                      pch = 20,
                      col = c("#67841E"),
                      cex = 1.0,
                      border_col = "grey30",
                      border_lwd = 0.5,
                      legend = FALSE,
                      legend_labs = levels(pts@data[,by]),
                      legend_position = c(1150405, 7532675)){
    stopifnot(class(pts) == "SpatialPointsDataFrame")
    temp <- data(list = basemap, package = "svamap")
    assign("background", get(temp))
    background <- spTransform(background, CRSobj = map_proj)
    pts <- spTransform(pts, CRSobj = map_proj)
    if(!is.null(by)){
        stopifnot(length(levels(pts@data[,by])) == length(col))
        colvec <- col[as.numeric(pts@data[,by])]
    } else {
        colvec <- col
    }
    pdf(path_to_file, height = height, width = width)
    ## basic map
    plot(background, lwd = border_lwd, border = border_col)
    ## Add points
    plot(pts, add = TRUE, pch=pch, cex = cex, col = colvec)
    if (legend & !is.null(by)){
        legend(x = legend_position[1], y = legend_position[2],
               legend=legend_labs,
               pch = pch, cex = 1.5*cex, col = col, bg = col,
               bty = "n")
    }
    dev.off()
    return(path_to_file)
}
