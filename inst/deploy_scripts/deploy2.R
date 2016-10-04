library(svamap)
##
data(NUTS_20M)
##
##Read in the point data
########################
pts <- read_point_data("/media/t/Falkenrapporter/E16-036 Grundrapport.csv")
##
## Drop the points that are not "Vilt (Jakt - fiske - natur)"
########################
## pts <- pts[pts$Djurhållning == "Vilt (Jakt - fiske - natur)",]
##
## Define the text to go in the popup for the point data
########################
pts$popup_text <- paste0(pts$Djurslag, "<br>Ankomstdatum: ", as.Date(pts$Ankomstdatum))
##
##Count points per polygon
########################
polys <- svamap::match_to_county(pts, NUTS_20M, "NUTS_ID")$polygons
##
## Drop data that you don't want to display from points
########################
pts@data <- data.frame(popup_text = pts@data$popup_text, stringsAsFactors = FALSE)
##
## Define popup text for polygons
########################
text <- ifelse(polys$count == 1, "negativt prov", "negativa prover")
polys@data$popup_text <- ifelse(is.na(polys@data$count),
                                polys@data$name,
                                paste0(polys@data$name, "<br>", polys@data$count, " ", text))
##
##Write data to geojson
########################
path_to_data <- svamap::write_data(list(polys, pts))
##
##Deploy map
########################
svamap::write_page(data = path_to_data,
                   path = "/media/ESS_webpages/CWD2/",
                   template = "map2",
                   overwrite = TRUE,
                   browse = FALSE)

##
##test Deploy map
########################
## svamap::write_page(data = path_to_data,
##                    path = "/tmp/",
##                    template = "map2",
##                    overwrite = TRUE,
##                    browse = TRUE)
