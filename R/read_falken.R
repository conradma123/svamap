##' Read a standard Falken data query (.csv format) 
##' 
##' 
##' @title read_falken
##' @author SVA
##' @return A data.frame object
##' @export
##' @param path The path to the file with data
##' 
read_falken <- function(path = system.file("sample_data_cwd.csv", package = "svamap")) {
  ## Read data in. Make columns as character types.
  df <- read.csv2(path, as.is = TRUE)
  
  ## Trim and replace all "" with NA
  for (i in seq_len(ncol(df))) {
    if(identical(is.character(df[, i]), TRUE)) {
      df[, i] <- sub("\\s+$", "", sub("^\\s+", "", df[, i]))
      df[nchar(df[, i]) == 0L, i] <- NA_character_
    }
  }
  
  ## Convert dates to Date class
  df$Ankomstdatum <- as.Date(df$Ankomstdatum)
  
  ## Add enhet
  df$Enhet <- substr(df$Provid, 4, 6)
  
  ## Add one column with the status TRUE/FALSE
  df$status <- ifelse(df$Resultat == "PÅVISAD", TRUE, FALSE)
  
  ## :TODO:FIX: Add check for multiple Djuridentitet and Provid
  
  return(df)
}
