##' Read in the sample data and clean it
##' 
##' @title read_data
##'
##' This function reads in data from SVA's database on CWD
##' submittions. The default is to read in a fake dataset that has the
##' same strucuture.
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
##' 
##' @return A data.frame
##' @author Thomas Rosendal
##' @import utils
##' @import sp
##' @import rgdal
##' @export
##' @param path Path to file
##' @param encoding Ecoding of the text file
##' @param proj4str projection of points in file
##' @param long text string of the variable name that is longitude
##' @param lat text string of the variable name that is latitude
read_data <- function(path = system.file("sample_data_cwd.csv", package = "svamap"),
                      encoding = "UTF-8",
                      proj4str = "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0 +units=m +no_defs",
                      long = "Gisy",
                      lat = "Gisx") {
    df <- read.csv2(path,
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    encoding = encoding)
    pts <- SpatialPoints(cbind(df[,long], df[,lat]))
    proj4string(pts) <- proj4str
    pts <- spTransform(pts, CRSobj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    pts <- SpatialPointsDataFrame(pts, df)
    return(pts)
}
##' Write a geojson file from a dataset
##' 
##' @title write_data 
##' @param object A spatial polygon or point dataframe
##' @param file 
##' @return 
##' @author Thomas Rosendal
write_data <- function(object,
                       file) {
    
}
