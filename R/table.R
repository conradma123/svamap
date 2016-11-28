##' html_table
##'
##' A function that takes a data.frame and writes to an html table
##'
##' @title html_table
##' @param df The dataframe to be written to html table form
##' @param html.table.attributes The attributes of the table. This is
##'     a character vector that is placed in the <table> tag. Should
##'     include the class(es) of the table. The default is an html
##'     table with class 'svatablegrayheader' which is one of the
##'     table classes defined in sva's main.css
##' @param html_head The head of the html. This is a character vector
##'     that contains the text to be put in the html <head> section of
##'     the page. The default calls the sva main.css
##' @param align The alignement of each column. This is a character
##'     vector that indicates the alignment of each column in the
##'     table. The default is all 'left'
##' @param col.names The names of the column heading in the
##'     table. This is a character vector of the names to be used in
##'     the resulting table. The default are the column names from df
##' @param file A path to a file. A valid path to write the html. The
##'     default is a tempfile().
##' @return A path to an html file
##' @author Thomas Rosendal
##' @import xtable
##' @export
html_table <- function(df,
                       html.table.attributes = 'class="svatablegrayheader" style="width: 100%;" border="0"',
                       html_head = c('<meta charset="utf-8" />',
                                     '<meta http-equiv="x-ua-compatible" content="IE=edge" >',
                                     '<link rel="stylesheet" href="http://www.sva.se/assets/css/main.css" />'),
                       align = rep('left', ncol(df)),
                       col.names = names(df),
                       file = tempfile()){
    if(!("data.frame" %in% class(df))){
        stop("Argument df must be a data.frame")
    }
    if(ncol(df) != length(col.names)){
        stop("col.names vector must be equal in length to the number of columns in df")
    }
    if(ncol(df) != length(align)){
        stop("The alignment argument must be length equal to the number of columns in df")
    }
    tab <- xtable(x = df, align = c("left", align))
    names(tab) <- col.names
    body <- print.xtable(tab,
                  type = "html",
                  include.rownames = FALSE,
                  html.table.attributes = html.table.attributes,
                  print.results = FALSE)
    prefix <- c('<!DOCTYPE html>', '<html>')
    suffix <- c('</html>')
    writeLines(c(prefix,
                 "<head>",
                 html_head,
                 "</head>",
                 "<body>",
                 body,
                 "</body>",
                 suffix),
               file)
    return(file)
}
