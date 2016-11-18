##' Generate a time series (xts) object
##' 
##' @title time_series_write
##'
##' This function reads in data from SVA's database and produce 
##' an xts object with the cumsum of the confirmed cases
##' 
##' @return A data.frame object
##' @author Giampaolo Cocca
##' @import utils
##' @import xts
##' @export
##' @param path Path to file
##' @param encoding Ecoding of the text file
##' @param date_in dataset field with dates
##' @param target numeric value to be plotted 
time_series_write <- function(path = system.file("sample_data_cwd.csv", package = "svamap"),
                             encoding = "UTF-8",
                             date_in = "Ankomstdatum",
                             target = "Status..numerisk.") {
  
  df <- read.csv2(path,
                  header = TRUE,
                  stringsAsFactors = FALSE,
                  encoding = encoding,
                  na.strings = c("NA", " ", ""))

  df <- df[!duplicated(df$Uppdragid), ]
  
  # test missing date
  missing_date <- length(which(!complete.cases(df[, date_in])))
  if(missing_date > 0){
    warning(paste(missing_date, "of the submitted records has/have missing date"))
  }
  
  # test date in wrong format
  df <- df[order(df[, date_in]),]
  date_format <- as.Date(df[, date_in])
  date_wrong <- length(which(!complete.cases(date_format)))
  if(date_wrong > 0){
    warning(paste(date_wrong, "of the submitted records has/have a date in the wrong format"))
  }

  value <- cumsum(df[, target])
  
  time_serie <- xts(value, date_format)
  
  return(time_serie)
}