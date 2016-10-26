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
##' @param map_proj The projection of those output map (represented in proj4string)
##' @param basemap The name of the basemap
##' @param path_to_file The output file 
##' @param width The width in inches
##' @param height The height in inches
##' @param pch The plotting character
##' @param col The col of the plotting character
##' @param cex The size of the plotting character
##' @return A path to the pdf file of the map
##' @author Thomas Rosendal
##' @import utils
##' @import sp
##' @import rgdal
##' @import stats
##' @import grDevices
##' @export
point_map <- function(pts,
                      map_proj = "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0 +units=m +no_defs",
                      basemap = "NUTS_20M",
                      path_to_file = tempfile(fileext = ".pdf"),
                      height = 7,
                      width = 5,
                      pch = 20,
                      col = "#67841E",
                      cex = 1.0){
    temp <- data(list = basemap, package = "svamap")
    assign("background", get(temp))
    background <- spTransform(background, CRSobj = map_proj)
    pts <- spTransform(pts, CRSobj = map_proj)
    pdf(path_to_file, height = height, width = width)
    ## basic map
    plot(background, lwd = 0.5, border = "grey30")
    ## Add points
    plot(pts, add = TRUE, pch=pch, cex = cex, col = col)
    dev.off()
    return(path_to_file)
}
