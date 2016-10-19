##' Generate a time series (xts) object
##' 
##' @title read_time_series
##'
##' This function reads in data from SVA's database and produce 
##' an xts object 
##' 
##' @return A data.frame
##' @author Giampaolo Cocca
##' @import utils
##' @import xts
##' @export
##' @param path Path to file
##' @param encoding Ecoding of the text file
##' @param date_in dataset field with dates
##' @param target numeric value to be plotted 
write_time_series <- function(path = system.file("sample_data_cwd.csv", package = "svamap"),
                             encoding = "UTF-8",
                             date_in = "Ankomstdatum",
                             target = "Djurslagskod") {
  
  df <- read.csv2(path,
                  header = TRUE,
                  stringsAsFactors = FALSE,
                  encoding = encoding)
  
  # test missing date
  missing_date <- length(which(!complete.cases(df[, date_in])))
  if(missing_date > 0){
    warning(paste(missing_date, "of the submitted records has/have missing date"))
  }
  
  # test date in wrong format
  date_format <- as.Date(df[, date_in], format= "%Y/%m/%d %H:%M:%S")
  date_wrong <- length(which(!complete.cases(date_format)))
  if(date_wrong > 0){
    warning(paste(date_wrong, "of the submitted records has/have a date in the wrong format"))
  }
  
  
  time <- as.POSIXct(paste(df$date, df$extime), format = "%Y-%m-%d %H:%M:%OS")
  value <- df[, target]
  
  time_serie <- xts(value, date_format)
  
  colnames(time_serie) <- target
  
  return(time_serie)
  
}