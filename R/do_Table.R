##' Create a an .html table with backgroundcolors
##' 
##' The function is built on the top of R "DT" package.
##' Package bind "DataTables" a plug-in for the jQuery Javascript library
##' 
##' @title do_Table
##' @import DT
##' @export
##' @param x an object of class data.frame
##' @param disease name string of the disease investigated
##' @param header character vector with the header of the table
##' @param tocolor character vector with the name of the field(s) to format
##' @param colorPal color to use as cells background
##' @param target vector on which apply the style
##' @param browse if TRUE it opens the map in a new page of the default browser
##' @param dir path where the map will be saved
##' @return path to the table
##' @author Giampaolo Cocca
do_Table <- function(x,
                     disease,
                     header,
                     tocolor,
                     colorPal,
                     target,
                     browse,
                     dir) {

  DTable <- datatable(x,
            colnames = header,
            options = list(dom = 't', 
                           columnDefs = list(list(
                             className = 'dt-center', targets = 1))),
            rownames = FALSE) %>%
    
    formatStyle(tocolor,
                backgroundColor = styleEqual(target, colorPal)) 
  
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
