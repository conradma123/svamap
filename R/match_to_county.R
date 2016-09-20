##' Function that takes data from point layer and applies it to the
##' appropriate polygons and returns a polygon layer with the chosen
##' summarized data
##'
##' @title match_to_county
##' @export
##' @param pts a spatial points dataframe
##' @param polygons and spatial polygons dataframe
##' @param id The id field in the polygondataset
##' @return A spatial polygon dataframe
##' @author Thomas Rosendal
match_to_county <- function(pts,
                            polygons,
                            id = "NUTS_ID") {
    pts$id <- over(pts, polygons, stringsAsFactors = FALSE)[,id]
    pts2 <- pts[is.na(pts$id),]
    pts <- pts[!is.na(pts$id),]
     df <- do.call("rbind", lapply(unique(pts$id), function(x) {
        data.frame("id" = x,
                   "count" = nrow(pts[pts$id == x,]),
                   stringsAsFactors = FALSE)
    }))
    polygons$count <- df$count[match(polygons@data[,id], df$id)]
    if(!nrow(pts2)==0){
        warning("Some of the points supplied are not contained within the polygon layer.\nThey will be in the second list item of the object returned from this function")
    }
    final <- list(polygons = polygons, outlier_pts = pts2)
    return(final)
}
