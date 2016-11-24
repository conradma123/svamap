library(svamap)
library(sp)
##
data(NUTS_20M)
##
##Read in the point data
########################
pts <- read_point_data("/media/t/Falkenrapporter/AI vilda fåglar.csv")
pts@data$Ankomstdatum <- as.Date(pts@data$Ankomstdatum)
pts <- pts[!is.na(pts@data$Aiv313),]
pts@data$Aiv313[pts@data$Aiv313 == "ej påvisad, ej påvisad"] <- "ej påvisad"
## Make sure the unique values in the Aiv313 vector are what we expect
stopifnot(identical(unique(pts@data$Aiv313), c("ej påvisad", "PÅVISAD", "")))
stopifnot(identical(unique(pts@data$Aivh5313), c("", "PÅVISAD", "ej påvisad")))
stopifnot(identical(unique(pts@data$Aivh7313), c("", "ej påvisad")))
stopifnot(FALSE)
pts@data$result <- ifelse(pts@data$Aiv313 == "PÅVISAD", 1,
                   ifelse(pts@data$Aiv313 == "ej påvisad", 0, 2))
pts@data$result <- as.integer(pts@data$result)
pts@data$popup_text <- paste("<b>Djurslag</b>", pts@data$Djurslag, "<br/>",
                             "<b>Datum</b>", pts@data$Ankomstdatum, "<br/>")
pts <- pts[pts@data$Ankomstdatum > "2016/09/01 00:00:00", ]
##drop duplicates
pts <- pts[!duplicated(pts@data$Namn), ]
## drop those positive on 'Matrix' but are negative for H5 and H7
pts <- pts[!(pts@data$result == 1 & !(pts@data$Aivh5313 == "PÅVISAD" | pts@data$Aivh7313 == "PÅVISAD")),]
pts@data <- data.frame(species = pts@data$Djurslag,
                       result = pts@data$result,
                       popup_text = pts@data$popup_text,
                       stringsAsFactors = FALSE)
## Drop if the sample is not yet complete:
pts <- pts[pts$result != 2,]
## sort by påvisade to get the positives plotted last
pts <- pts[order(pts@data$result),]
##Write data to geojson
########################
path_to_data <- svamap::write_data(pts)
########################
##Deploy map to internal server
########################
svamap::write_page(data = path_to_data,
                   path = "/media/ESS_webpages/AI/",
                   template = "influenza",
                   overwrite = TRUE,
                   browse = FALSE)
##
## Deploy map to Azure server. This is SVA's external website and is
## administered by a company "Episerver hosting" the contact for this
## company at SVA is the communications department.
temp <- readLines("~/.svaftp_credentials")
cred <- paste0("ftp://", temp[2], ":", temp[3], "@", temp[1], "/MAPS/AI/")
svamap::write_page(data = path_to_data,
                   template = "influenza",
                   overwrite = TRUE,
                   browse = FALSE,
                   ftp = cred)
##test Deploy map
########################
## svamap::write_page(data = path_to_data,
##                    path = "/tmp/",
##                    template = "map3",
##                    overwrite = TRUE,
##                    browse = TRUE)
