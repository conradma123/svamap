##' Create a browsable choropleth map
##' 
##' The function is built on the top of R "leaflet" and "mapview" packages.
##' Both packages bind the JavaScript library Leaflet.js)
##' 
##' @title choropleaf_map
##' @import leaflet
##' @import htmlwidgets
##' @import mapview
##' @export
##' @param mapdata an object of class SpatialPolygonsdataFrame
##' @param values vector of values of class "numeric" used to fill the polygon
##' @param palette colors to be assigned to each of the values
##' @param title the title of the legend
##' @param labels a vector of text labels to be displayed in the legend corresponding to palette
##' @param popup text to be showed in the popup when clicking a polygon
##' @param logo logo to put in the topleft corner of the map
##' @param src character specifying the source location of the logo ("local" for images from the disk, "remote" for web image sources)
##' @param url the url to show when clicking the logo
##' @param dir path where the map will be saved
##' @param disease name of the html file. The suffix "_map.html" will be added to the provided name
##' @param browse if TRUE it opens the map in a new page of the default browser
##' @return path to the map
##' @author Giampaolo Cocca
choropleaf_map <- function(mapdata,
                           values, 
                           palette,
                           title = NULL,
                           labels,
                           popup = NULL,
                           logo = "https://assets-cdn.github.com/images/modules/logos_page/GitHub-Logo.png",
                           src = "remote",
                           url = "https://github.com/",
                           dir = tempdir(),
                           disease = "myDisease",
                           browse = FALSE) {
  
  # Exceptions
  stopifnot(class(mapdata) %in% c("SpatialPolygonsDataFrame"))
  
  if(missing(values)) {
    stop("Argument 'values' is missing with no default")
  }
  
  if(missing(palette)) {
    stop("Argument 'palette' is missing with no default")
  }
  
  if(missing(labels)) {
    stop("Argument 'labels' is missing with no default")
  }
  
  if(class(values) != "numeric") {
    stop("Argument 'values' must be of class numeric")
  }
  
  if(class(labels) != "character") {
    stop("Argument 'labels' must be of class character")
  }
  
  if(length(unique(values)) != length(palette)){
    stop("values' and 'palette' must be of the same length!")
  }
  
  if(length(palette) != length(labels)){
    stop("colors' and 'palette' must be of the same length!")
  }
  
 
  # Build the leaflet map
  pal <- colorNumeric(palette = palette,
                      domain = values, na.color = NA) 
  
  
  leaf <- leaflet() %>%  addTiles() %>%
    # Add logo SVA  
    addLogo(img = logo, src="remote", position = "topleft", height= 43, width = 270, url = url) %>%   
    addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Vägkarta") %>%
    addProviderTiles("Esri.WorldTopoMap", group = "Terräng") %>%
    addProviderTiles("Esri.WorldImagery", group = "Flygfoto") %>%
    setMaxBounds(10.96139, 54.33625, 24.17240, 69.05904) %>%
    
    addPolygons(data = mapdata, 
                stroke = TRUE,
                color = "grey",
                weight = 0.5,
                opacity = 0.5,
                fillColor = ~pal(values),
                fillOpacity = 0.7,
                popup = popup,
                group = disease) %>%
    
    # "&nbsp" is used to escape whitespaces in html. Did that to move the legend title.
    addLegend("bottomright", 
              values = values,
              colors = palette,
              title = paste0(paste0(rep("&nbsp", 7), collapse = ""), 
                             "<sup>", "Last update ", as.character(Sys.time()), "</sup>","<br>",
                             paste0(rep("&nbsp", 7), collapse = ""), title),
              labels = labels,
              opacity = 0.7) %>%
    
    addLayersControl(baseGroups = c("Vägkarta", "Terräng", "Flygfoto"),
                     overlayGroups = disease,
                     options = layersControlOptions(collapsed = TRUE))
  
  # Path where to save the map
  myfile <- file.path(dir, paste0(disease, "_map.html"))
  
  # Save the map
  saveWidget(leaf, file = myfile, selfcontained = FALSE)
  
  # Read and write lines to add the support for reading the map from IE (change to IE Edge)
  lines <- readLines(myfile, skipNul = TRUE)
  
  header <- c("<!DOCTYPE html>",
              "<html>",
              "<head>",
              "<meta http-equiv=\"x-ua-compatible\" content=\"IE=edge\" >",
              "<meta charset=\"utf-8\"/>")
  
  newLines <- c(header, lines[5:22])
  
  writeLines(newLines, con = myfile)
  
  if(browse) {
    browseURL(paste0("file://", myfile))
  }
  
  return(myfile)
}