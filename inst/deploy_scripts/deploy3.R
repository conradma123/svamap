library(svamap)
library(sp)
##
data(NUTS_20M)
##
##Read in the point data
########################
pts <- read_point_data("/media/t/Falkenrapporter/E16-036 Grundrapport.csv")
##########################
## REMOVE ALL POSITIVES###
##########################
## This is done in order to prevent results being published prior to a
## press release. We will discuss individual positives and when to add
## them to a map as needed
pts <- pts[pts@data$Status..numerisk. == 0,]
##pts <- read_point_data()
##
## Drop the points that are not "Vilt (Jakt - fiske - natur)"
########################
pts <- pts[pts$Djurhållning == "Vilt (Jakt - fiske - natur)",]
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
##Deploy map
########################
svamap::write_page(data = path_to_data,
                   path = "/media/ESS_webpages/CWD3/",
                   template = "map3",
                   overwrite = TRUE,
                   browse = FALSE)

##test Deploy map
########################
## svamap::write_page(data = path_to_data,
##                    path = "/tmp/",
##                    template = "map3",
##                    overwrite = TRUE,
##                    browse = TRUE)
