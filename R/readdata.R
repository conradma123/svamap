##' Read in point data and clean it
##'
##' @title read_point_data
##'
##' This function reads in data from SVA's database on CWD
##' submittions. The default is to read in a fake dataset that has the
##' same strucuture.
##'
##' FYI the following are some proj4string specs. RT90 is the default
##'
##' RT90 is
##'
##' "+init=epsg:3021
##' +proj=tmerc +lat_0=0
##' +lon_0=15.80827777777778
##' +k= ##' 1
##' +x_0=1500000
##' +y_0=0 +ellps=bessel
##' +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0
##' +units=m
##' +no_defs"
##'
##' SWEREF99 is
##'
##' "+proj=utm
##' +zone=33
##' +ellps=GRS80
##' +towgs84=0,0,0,0,0,0,0
##' +units=m
##' +no_defs"
##'
##' WGS84 is
##'
##' "+proj=longlat
##' +ellps=WGS84
##' +datum=WGS84
##' +no_defs"
##'
##' @return A SpatialPointsDataframe
##' @author Thomas Rosendal
##' @import utils
##' @import sp
##' @import rgdal
##' @importFrom stats complete.cases
##' @export
##' @param path Path to file
##' @param encoding Encoding of the text file
##' @param proj4str projection of points in file
##' @param output_proj the projection of the returned dataframe (default WGS84)
##' @param long text string of the variable name that is longitude
##' @param lat text string of the variable name that is latitude
read_point_data <- function(path = system.file("sample_data_cwd.csv",
                                               package = "svamap"),
                            encoding = "UTF-8",
                            proj4str = paste0(c("+init=epsg:3021",
                                                "+proj=tmerc",
                                                "+lat_0=0",
                                                "+lon_0=15.80827777777778",
                                                "+k=1",
                                                "+x_0=1500000",
                                                "+y_0=0 +ellps=bessel",
                                                "+towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0",
                                                "+units=m",
                                                "+no_defs"), collapse = " "),
                            output_proj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                            long = "Gisy",
                            lat = "Gisx") {
    df <- read.csv2(path,
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    encoding = encoding)
    missing_coords <- length(which(!complete.cases(df[,c(long,lat)])))
    if(missing_coords > 0){
        warning(paste(missing_coords, "of the submitted points are missing coordinates"))
    }
    df <- df[complete.cases(df[,c(long,lat)]),]
    pts <- SpatialPoints(cbind(df[,long], df[,lat]))
    proj4string(pts) <- proj4str
    pts <- spTransform(pts, CRSobj = output_proj)
    pts <- SpatialPointsDataFrame(pts, df)
    return(pts)
}

##' Write a geojson file from a dataset
##'
##' @title write_data
##' @import rgeos
##' @export
##' @param object A list of spatial polygon or point dataframes list may be of length 1
##' @param varname The name of the variable to be given to the geojson oject in javascript.
##' @param file The path to where the geojson will be written
##' @return path to the file
##' @author Thomas Rosendal
write_data <- function(object,
                       varname = "data",
                       file = tempfile()) {
    stopifnot(all(class(object) %in% c("list",
                                   "SpatialPointsDataFrame",
                                   "SpatialLinesDataFrame",
                                   "SpatialPolygonsDataFrame"))
              )
    if(class(object) != "list") {
        object <- list(object)
    }
    time <- paste0("timestamp = \"", as.character(Sys.time()), "\"")
    js <- do.call('c', lapply(seq_len(length(object)), function(x){
        innerfile <- tempfile()
        writeOGR(object[[x]],
                 innerfile,
                 layer = "main",
                 driver = "GeoJSON",
                 check_exists = FALSE)
        geojson <- readLines(innerfile)
        js <- c(paste0(varname,x), " = ", geojson)
    }))
    js <- c(time, js)
    writeLines(js, file)
    return(file)
}
##' write a webpage
##'
##' Copy the files to a direcetory
##' @title write_page
##' @export
##' @param data path to data
##' @param path path to new webpage does not end in a "/"
##' @param ftp NULL if you want to you a path to a directory,
##'     otherwise specify the ftp server with credentials like:
##'     "ftp://User:Password@servername/Destination". This is the
##'     directory on the ftp server that the files will be placed. It
##'     must end in a '/' and will be expanded when called by
##'     ftpUpload from the RCurl library.
##' @param template the name of the map template. This is a .html file
##'     in the inst directory of svamap
##' @param owntemplate path to you own template. This would include
##'     everything your map needs but the data including the assets,
##'     like .css, .js and .png files
##' @param overwrite Do you want to overwrite files in the destination
##'     directory?
##' @param browse Pop the brwoser and view the page?
##' @return The path to the map or NULL if ftp
##' @import RCurl
##' @author Thomas Rosendal
write_page <- function(data,
                       path = tempdir(),
                       ftp = NULL,
                       template = "CWD/map.html",
                       owntemplate = NULL,
                       overwrite = FALSE,
                       browse = TRUE) {
    if(is.null(owntemplate)) {
        mapfile <- system.file(template, package = "svamap")
        assets <- system.file(file.path("assets", assets(readLines(mapfile))), package = "svamap")
        from <- c(mapfile, assets)
    } else {
        mapfile <- owntemplate
        assets <- system.file(file.path("assets", assets(readLines(mapfile))), package = "svamap")
        from <- c(mapfile, assets)
    }
    if(is.null(ftp)) {
        pathmap <- file.path(path, "map")
        dir.create(pathmap, showWarnings = FALSE)
        file.copy(from = data,
                  to = file.path(pathmap,"data.js"),
                  overwrite = overwrite)
        file.copy(from = from,
                  to = pathmap,
                  overwrite = overwrite,
                  recursive = FALSE)
        if(browse) {
            browseURL(paste0("file://", file.path(pathmap, "map.html")))
        }
        return(path)
    } else {
        ftpUpload(data, paste0(ftp, "data.js"))
        for (i in from) {
            ftpUpload(i, paste0(ftp, basename(i)))
        }
    }
    return(NULL)
}

##' Assets
##'
##' @title assets
##' @param l the lines of an html file
##' @return a vector of the css, js and png files called locally in the file
##' @author Thomas Rosendal
##' @export
assets <- function(l) {

    ## Get the js source files that are needed except those that are read remotely
    ## And not the file named data.js because that is added separately
    l1 <- l[grep("[[:space:]]*<script[[:space:]]src=", l)]
    l1 <- gsub("[[:space:]]*<script[[:space:]]src=\"([^\"]*).*$", "\\1", l1)
    l1 <- l1[!grepl("http://", l1)]
    l1 <- l1[!grepl("data.js", l1)]

    ## Get the css files that are needed except those that are read remotely
    l2 <- l[grep("[[:space:]]*<link[[:space:]]rel=\"stylesheet\"[[:space:]]href=", l)]
    l2 <- gsub("[[:space:]]*<link[[:space:]]rel=\"stylesheet\"[[:space:]]href=\"([^\"]*).*$", "\\1", l2)
    l2 <- l2[!grepl("http://", l2)]

    ## Check for any needed png file (icons)
    l3 <- l[grep("src[[:space:]]?=[[:space:]]?.*[.]png", l)]
    l3 <- gsub(".*src[[:space:]]?=[[:space:]]?\"(.*[.]png).*$", "\\1", l3)
    l3 <- l3[!grepl("http://", l3)]

    return(c(l1, l2, l3))
}
