##' Generate a time series (xts) object
##' 
##' @title write_time_series
##'
##' This function reads in data from SVA's database and produce 
##' an xts object 
##' 
##' @return A data.frame object
##' @author Giampaolo Cocca
##' @import utils
##' @import xts
##' @param path Path to file
##' @param encoding Ecoding of the text file
##' @param date_in dataset field with dates
##' @param target numeric column(s) to be included in the xts object
##' @param name name to assign to the target column(s) in the xts object
##' @export
write_time_series <- function(df,
                              date_in,
                              target,
                              name) {
  # Exceptions
  stopifnot(class(df) %in% "data.frame")

  if(missing(date_in)) {
    stop("date_in' is missing with no default")
  }

  if(missing(target)) {
    stop("target' is missing with no default")
  }
  
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
  
  value <- df[, target]
  
  time_serie <- xts(value, date_format)
  
  colnames(time_serie) <- target
  
  return(time_serie)
  
}
