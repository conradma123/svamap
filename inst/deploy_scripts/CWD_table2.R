library(svamap)
library(hlt)
library(RCurl)
##
data(rough_lan)
##
##Read in the data
pts <- read_point_data("/media/t/Falkenrapporter/E16-036 Grundrapport.csv")
##
pts@data$Publicera <- factor(pts@data$Publicera, levels = c("Ja", "Nej"))
##
##Now keep all negatives unless Publicera is "Nej"; Drop all Positives unless Publicera is "Ja"
##
pts <- pts[(pts@data$Status..numerisk. == 0 &
            (pts@data$Publicera != "Nej" | is.na(pts@data$Publicera))
           ) |
           (pts@data$Publicera == "Ja" & !is.na(pts@data$Publicera)),]
## Drop the points that are not "Vilt (Jakt - fiske - natur)"
########################
pts <- pts[pts$Djurhållning == "Vilt (Jakt - fiske - natur)" & !is.na(pts$Djurhållning),]
########################
##
##Count points per polygon
##
########################
## Project to planar
polys <- svamap::match_to_county(pts, rough_lan, "NUTS_ID")
polys <- polys[[1]]
## Just keep the basic info for the table
df <- polys@data[,c("name", "count")]
df$count <- as.integer(df$count)
df$count[is.na(df$count)] <- 0
total <- sum(df$count)
df <- rbind(df, c("Total", total))
names(df) <- c("Län", "Antal undersökta")
############################
##
## Generate a valid header for the HTML
##
############################
head <- hlt::html_head(
                 hlt::html_comment(paste("Page generated with\n'hlt' version:", packageVersion("hlt"), "\n svamap version:", packageVersion("svamap"), "\nat:",Sys.time())) +
                 hlt::html_meta(charset = "utf-8") +
                 hlt::html_meta("http-equiv" = "x-ua-compatible", content="IE=edge") +
                 hlt::html_meta(NAME = "ROBOTS", CONTENT = "NOINDEX, NOFOLLOW") +
                 hlt::html_link(rel = "stylesheet", href = "http://www.sva.se/assets/css/main.css") +
                 hlt::html_link(rel = "stylesheet", type = "text/css", href ="https://cdn.datatables.net/1.10.12/css/jquery.dataTables.min.css") +
                 hlt::html_script(content = "", type="text/javascript", charset="utf8", src="https://code.jquery.com/jquery-1.12.3.js") +
                 hlt::html_script(content = "", type="text/javascript", charset="utf8", src="https://cdn.datatables.net/1.10.12/js/jquery.dataTables.min.js") +
                 hlt::html_script(content = paste(c("$(document).ready(function() {",
                                                    "$('#table1').DataTable({",
                                                    "    'paging': false,",
                                                    "    'ordering': true,",
                                                    "    'info': false,",
                                                    "    'searching': false",
                                                    "} );",
                                                    "} );"), collapse = "\n")
                                  )
             )
#############################
##
## Generate an HTML table:
##
############################
table <- hlt::html_table(df, tfoot = TRUE)
## Add some attributes to the table and alignment to the cells:
hlt::tag_attr(table) <- list(id = "table1", class = "svatablegrayheader", style = "width:100%;", border = "0")
for(i in seq_len(length(table$content[[3]]$content))) {
        hlt::tag_attr(table$content[[3]]$content[[i]]$content[[2]]) <- list(align = "right")
}
page <- hlt::html_html(head + hlt::html_body(table))
tab <- tempfile()
capture.output(file = tab, print(page))
## Deploy map to Azure server. This is SVA's external website and is
## administered by a company "Episerver hosting" the contact for this
## company at SVA is the communications department.
temp <- readLines("~/.svaftp_credentials")
cred <- paste0("ftp://", temp[2], ":", temp[3], "@", temp[1], "/MAPS/CWD/")
ftpUpload(tab, paste0(cred, "table2.html"))
