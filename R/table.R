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
##' @param html_head An object of class svamap.htmlheader. See generate_header() function
##' @param align The alignement of each column. This is a character
##'     vector that indicates the alignment of each column in the
##'     table. The default is all 'left'
##' @param col.names The names of the column heading in the
##'     table. This is a character vector of the names to be used in
##'     the resulting table. The default are the column names from df
##' @param file A path to a file. A valid path to write the html. The
##'     default is a tempfile().
##' @param fragment Do you want just a fragment or a full html?
##' @param footer Do you want the last row of the table to be in a '<tfoot>' tag?
##' @return A path to an html file
##' @author Thomas Rosendal
##' @import xtable
##' @export
html_table <- function(df,
                       html.table.attributes = 'id = "table1" class="svatablegrayheader" style="width: 100%;" border="0"',
                       html_head = generate_header(),
                       align = rep('left', ncol(df)),
                       col.names = names(df),
                       file = tempfile(),
                       fragment = FALSE,
                       footer = FALSE){
    if(!("data.frame" %in% class(df))){
        stop("Argument df must be a data.frame")
    }
    if(class(html.table.attributes) != "character"){
        stop("html.table.attributes must be a character vector")
    }
    if(!(identical(class(html_head), "svamap.htmlheader"))){
        stop("the html_head must be of class 'svamap.htmlheader'. see generate_header() function")
    }
    if(ncol(df) != length(col.names)){
        stop("col.names vector must be equal in length to the number of columns in df")
    }
    if(ncol(df) != length(align)){
        stop("The alignment argument must be length equal to the number of columns in df")
    }
    ## Produce the html table with xtable
    tab <- xtable(x = df, align = c("left", align))
    names(tab) <- col.names
    ## Write the table html and add the <thead> and <tbody> elements
    ## to allow the table to referenced in this why in .css
    body <- print.xtable(tab,
                  type = "html",
                  include.rownames = FALSE,
                  html.table.attributes = html.table.attributes,
                  print.results = FALSE,
                  comment = FALSE)
    body <- unlist(strsplit(body, "\n"))
    thead <- c(body[1:grep("<table", body)[1]],
               "<thead>",
               body[grep("<table", body)+1],
               "</thead>")
    if(footer){
    tbody <- c("<tbody>",
               body[(grep("<table", body) + 2) : (grep("</table", body) -2)],
               "</tbody>")
    tfoot <- c("<tfoot>",
               body[(grep("</table", body) - 1) : (grep("</table", body) -1)],
               "</tfoot>")
    } else {
    tbody <- c("<tbody>",
               body[(grep("<table", body) + 2) : (grep("</table", body) -1)],
               "</tbody>")
    tfoot <- c("<tfoot>",
               "",
               "</tfoot>")
    }
    body <- c(thead, tbody, tfoot, "</table>")
    prefix <- c('<!DOCTYPE html>', '<html>')
    suffix <- c('</html>')
    ## Stick the entire page together and write it to a text file
    if(fragment){
        final <- body
    } else {
    final <- c(prefix,
               "<head>",
               html_head,
               "</head>",
               "<body>",
               body,
               "</body>",
               suffix)
    }
    if(is.null(file)){
        return(final)
    }else {
        writeLines(final, file)
        return(file)
    }
}

##' A function that generates the header for the html table
##'
##' @title generate header
##' @param beginning A free set of text lines. This is a character
##'     vector that contains the text to be put in the html <head>
##'     section of the page. The default calls the sva main.css and
##'     makes the page compatible with IE and sets text encoding to
##'     UTF-8
##' @param otherstuff Whatever otherlines of header you want to
##'     add. This will be placed after 'beginning' and before the
##'     javascript table code.
##' @param paging Either TRUE or FALSE. Do you want the table to have
##'     paging?
##' @param ordering Either TRUE or FALSE Do you want sorting?
##' @param info Either TRUE or FALSE Do you want info?
##' @param searching Either TRUE or FALSE Do you want a search field?
##' @return A header object to be passed to the html_table function
##' @author Thomas Rosendal
##' @export
generate_header <- function(beginning = c('<meta charset="utf-8" />',
                                          '<meta http-equiv="x-ua-compatible" content="IE=edge" >',
                                          '<link rel="stylesheet" href="http://www.sva.se/assets/css/main.css" />'),
                            otherstuff = NULL,
                            paging = FALSE,
                            ordering = FALSE,
                            info = FALSE,
                            searching = FALSE){
    if(class(beginning) != "character"){
        stop("beginning must be a character vector")
    }
    if(!is.logical(paging)){
        stop("paging must be TRUE or FALSE")
    }
    if(!is.logical(ordering)){
        stop("ordering must be TRUE or FALSE")
    }
    if(!is.logical(info)){
        stop("info must be TRUE or FALSE")
    }
    if(!is.logical(searching)){
        stop("searching must be TRUE or FALSE")
    }
    ## Add the js and css libraries if we need data.table
    if(any(c(paging, ordering, info, searching))){
        data_table_scripts <- c('<link rel="stylesheet" type="text/css" href="https://cdn.datatables.net/1.10.12/css/jquery.dataTables.min.css">',
                                '<script type="text/javascript" charset="utf8" src="https://code.jquery.com/jquery-1.12.3.js"></script>',
                                '<script type="text/javascript" charset="utf8" src="https://cdn.datatables.net/1.10.12/js/jquery.dataTables.min.js"></script>')
        if(paging){
            paging = '  "paging": true,'
        } else {
            paging = '  "paging": false,'
        }
        if(ordering){
            ordering = '  "ordering": true,'
        } else {
            ordering = '  "ordering": false,'
        }
        if(info){
            info = '  "info": true,'
        } else {
            info = '  "info": false,'
        }
        if(searching){
            searching = '  "searching": true'
        } else {
            searching = '  "searching": false'
        }
        final <- c(beginning,
                   otherstuff,
                   data_table_scripts,
                   '<script>',
                   '$(document).ready(function() {',
                   "$('#table1').DataTable({",
                   paging,
                   ordering,
                   info,
                   searching,
                   '} );',
                   '} );',
                   '</script>')
        class(final) <- "svamap.htmlheader"
        return(final)
    }
    final <- c(beginning, otherstuff)
    class(final) <- "svamap.htmlheader"
    return(final)
}
