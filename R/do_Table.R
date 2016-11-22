##' Create a an .html table with custom style
##' 
##' The function is built on the top of R "DT" package.
##' Package bind "DataTables" a plug-in for the jQuery Javascript library
##' 
##' @title do_Table
##' @import DT
##' @export
##' @param x an object of class data.frame
##' @param disease name string of the disease investigated
##' @param header a character vector with the header of the table
##' @param targets integer number defining position of the column to center. First column is 0
##' @param lengthpage number of records to display per page
##' @param tocolor the indices of the columns to be formatted (can be character, numeric, logical, or a formula of the form ~ V1 + V2, which is equivalent to c('V1', 'V2'))
##' @param colorPal a vector of color to use as cells' background
##' @param targetcol a character vector of data values to be assigned (one-to-one) to 'colorPal'
##' @param initComplete JS snippet that modify background color and text color of the header 
##' @param width the width of the table in pixel
##' @param height the height of the table in pixel
##' @param browse if TRUE it opens the map in a new page of the default browser
##' @param dir path where the map will be saved
##' @return path to the table
##' @author Giampaolo Cocca
do_Table <- function(x,
                     disease = "myDisease",
                     header,
                     targets = NULL,
                     lengthpage = NULL,
                     tocolor = NULL,
                     colorPal = NULL,
                     targetcol = NULL,
                     initComplete = NULL,
                     height = NULL,
                     width = NULL,
                     browse = FALSE,
                     dir) {

  # Exceptions
  stopifnot(class(x) %in% c("data.frame"))
  
  if(class(disease) != "character") {
    stop("Argument 'disease' must be of class character")
  }
  
  DTable <- datatable(x,
            colnames = header,
            width = width,
            height = height,
            rownames = FALSE,
            options = list(pageLength = lengthpage,
                           dom = 't', 
                           columnDefs = list(list(
                             className = 'dt-center', targets = targets)),
                           initComplete = JS(initComplete),
            fillContainer = FALSE)) 
 
  if(missing(targetcol)){

     DTable <- formatStyle(table = DTable, columns = tocolor, backgroundColor = colorPal)
        
        }else{
     
     DTable <- formatStyle(table = DTable, columns = tocolor,
                           target = 'row', backgroundColor = styleEqual(targetcol, colorPal))
  }

  
  #Path to the table
  myfile <- file.path(dir, paste0(disease, "_table.html"))
  
  # Save the table
  saveWidget(DTable, file = myfile, selfcontained = FALSE)
  
  # Read and write lines to add the support for reading the map from IE (change to IE Edge)
  lines <- readLines(myfile, skipNul = TRUE)
  
  header <- c("<!DOCTYPE html>",
              "<html>",
              "<head>",
              "<meta http-equiv=\"x-ua-compatible\" content=\"IE=edge\" >",
              "<meta charset=\"utf-8\"/>")
  
  newLines <- c(header, lines[5:20])
  
  writeLines(newLines, con = myfile)
  
  if(browse) {
    browseURL(paste0("file://", myfile))
  }
  
  return(myfile)
}
