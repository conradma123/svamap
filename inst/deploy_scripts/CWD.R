library(svamap)
library(sp)
##
data(NUTS_20M)
##
##Read in the point data
########################
pts <- read_point_data("/media/t/Falkenrapporter/E16-036 Grundrapport.csv")
########################################################
## REMOVE ALL POSITIVES if Publicera flagg isn't "Ja"###
########################################################
##
## Keep only values "Ja" and "Nej"
pts@data$Publicera <- factor(pts@data$Publicera, levels = c("Ja", "Nej"))
##
##Now keep all negatives unless Publicera is "Nej"; Drop all Positives unless Publicera is "Ja"
##
pts <- pts[(pts@data$Status..numerisk. == 0 &
            (pts@data$Publicera != "Nej" | is.na(pts@data$Publicera))
           ) |
           (pts@data$Publicera == "Ja" & !is.na(pts@data$Publicera)),]
##pts <- read_point_data()
##
## Drop the points that are not "Vilt (Jakt - fiske - natur)"
########################
pts <- pts[pts$Djurhållning == "Vilt (Jakt - fiske - natur)" & !is.na(pts$Djurhållning),]
##
## Define the text to go in the popup for the point data
########################
pts$popup_text <- paste0(pts$Djurslag,
                         "<br>Ankomstdatum: ", as.Date(pts$Ankomstdatum),
                         "<br>Djuridentitet: ", pts@data$Djuridentitet)
## Drop data that you don't want to display from points
########################
pts@data <- data.frame(species = pts@data$Djurslag,
                       result = pts@data$Status..numerisk.,
                       popup_text = pts@data$popup_text, stringsAsFactors = FALSE)
##
moose <- pts[pts@data$species == "Älg",]
deer <- pts[pts@data$species == "Rådjur",]
kronhjort <- pts[pts@data$species == "Kronhjort",]
##Write data to geojson
########################
path_to_data <- svamap::write_data(list(pts, moose, deer, kronhjort))
##
##Deploy map to internal server
########################
svamap::write_page(data = path_to_data,
                   path = "/media/ESS_webpages/CWD3/",
                   template = "CWD/map.html",
                   overwrite = TRUE,
                   browse = FALSE)
##
## Deploy map to Azure server. This is SVA's external website and is
## administered by a company "Episerver hosting" the contact for this
## company at SVA is the communications department.
temp <- readLines("~/.svaftp_credentials")
cred <- paste0("ftp://", temp[2], ":", temp[3], "@", temp[1], "/MAPS/CWD/")
svamap::write_page(data = path_to_data,
                   template = "CWD/map.html",
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
