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
##' @export
##' @param path Path to file
##' @param encoding Ecoding of the text file
##' @param date_in dataset field with dates
##' @param target numeric value to be plotted 


time_series_graph <- function(xts_object = write_time_series()[[2]],
                              target = colnames(write_time_series()[[2]][,1])){

x <- dygraph(xts_object) 
  dySeries(x, target, label = target, strokePattern = "dashed", color="red") %>% 
  dyRangeSelector(height = 50) %>% 
  dyAxis("y", label = paste(target, " (n.)")) %>% 
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(axisLineWidth = 2.5, fillGraph = TRUE, fillAlpha = 0.5)

  }

