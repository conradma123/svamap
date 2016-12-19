##' Create a time series graph using dygraphs.js library
##' 
##' @title time_series_graph
##'
##' This function creates a time series graph using dygraphs.js
##' available in R through \code{library(dygraphs)}
##'
##' 
##' @return A data.frame
##' @author Giampaolo Cocca
##' @import utils
##' @import dygraphs
##' @import xts
##' @param xts_obj Object of type xts. Possibly result from \code{svamap::write_time_series}
##' @param target numeric vector to plot 
##' @param label character string to assign to y-axis and tooltip of the graph
##' @param dir path where the map will be saved
##' @param disease name of the html file. The suffix "_map.html" will be added to the provided name
##' @param browse if TRUE it opens the map in a new page of the default browser
##' @export
time_series_graph <- function(xts_obj,
                              target,
                              label,
                              dir = tempdir(),
                              disease = "my_disease",
                              browse = FALSE) {
  
  # Exceptions
  stopifnot(class(xts_obj) %in% c("xts", "zoo"))
  
  if(missing(target)) {
    stop("target' is missing with no default")
  }
  
  if(missing(label)) {
    stop("label' is missing with no default")
  }
  
  # Draw the graph after converting the dataframe to a 'dygraph' object
  x <- dygraph(xts_obj) 
  x_ts <- dySeries(x, name = target, label = label, strokePattern = "dashed", color="red") %>% 
    dyRangeSelector(height = 50) %>% 
    dyAxis(name = "y", label = paste(label, " (n.)"), drawGrid = TRUE) %>%
    dyOptions(axisLineWidth = 2.5, fillGraph = TRUE, fillAlpha = 0.5)
  
  # Path where to save the map
  myfile <- file.path(dir, paste0(disease, "_ts.html"))
  
  # Save the map
  saveWidget(x_ts, file = myfile, selfcontained = FALSE)
  
  # Read and write lines to add the support for reading the map from IE (change to IE Edge)
  lines <- readLines(myfile, skipNul = TRUE)
  
  header <- c("<!DOCTYPE html>",
              "<html>",
              "<head>",
              "<meta http-equiv=\"x-ua-compatible\" content=\"IE=edge\" >",
              "<meta charset=\"utf-8\"/>")
  
  newLines <- c(header, lines[length(header):max(length(lines))])
  
  writeLines(newLines, con = myfile)
  
  if(browse) {
    browseURL(paste0("file://", myfile))
  }
  
  return(myfile)
}
