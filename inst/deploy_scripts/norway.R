## Read data from website
library(XML)
df <- readHTMLTable("http://www.hjorteviltregisteret.no/HelseInnsyn/StatistikkCWD", which = 1, stringsAsFactors = FALSE)
## Keep just kommune
kommune <- df[trimws(df$Fylke) == "",]
library(rgdal)
colours <- c('#f7f7f7','#d9d9d9','#bdbdbd','#969696','#636363','#252525')
## Get the map of norway kommune
kommune_map <- readOGR("/media/trosendal/OS/projects/small_projects/CWD_norge/abas/kommuner.geojson",
                       "OGRGeoJSON", stringsAsFactors = FALSE)
data <- data.frame(kommune_map)
## Simplify the geometry a little
library(rmapshaper)
kommune_map <- ms_simplify(kommune_map, keep_shapes = TRUE, keep = 0.01)
kommune_map <- SpatialPolygonsDataFrame(kommune_map, data)
## match the data
kommune_map@data$result <- kommune$Totalt[match(kommune_map@data$navn, kommune$Kommune)]
kommune_map@data$positive <- kommune$Positive[match(kommune_map@data$navn, kommune$Kommune)]
## Assign colours by counts
q <- quantile(as.numeric(kommune_map@data$result),
              na.rm = TRUE,
              probs = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1))
kommune_map@data$colour <- colours[as.numeric(as.character(cut(as.numeric(kommune_map@data$result),
                                                               q,
                                                               include.lowest = TRUE, labels = c(1:6))
                                                           )
                                              )]
kommune_map$popup_text <- paste0(kommune_map$navn,
                                 "<br>Total number tested: ", kommune_map@data$result,
                                 "<br>Total number Positive: ", kommune_map@data$positive)
## make positive kommuner red
kommune_map@data$colour[which(as.numeric(kommune_map@data$positive)>0)] <- "#D22630"
## Creat popup text
kommune_map@data <- kommune_map@data[,names(kommune_map@data) %in% c("navn", "result", "colour", "popup_text")]
## plot map
library(svamap)
path_to_data <- svamap::write_data(list(kommune_map))
svamap::write_page(data = path_to_data,
                    path = "/media/ESS_webpages/Norway_CWD/",
                    template = "Norway",
                    overwrite = TRUE,
                    browse = FALSE)

#### Fylke is too big to be meaningful
## fylke <- df[trimws(df$Fylke) != "",]
## fylke_map <- readOGR("abas/fylker.geojson", "OGRGeoJSON", stringsAsFactors = FALSE)
## fylke_map@data$result <- fylke$Totalt[match(fylke_map@data$navn, fylke$Fylke)]
## q <- quantile(as.numeric(fylke_map@data$result), na.rm = TRUE, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.8,1))
## fylke_map@data$colour <- colours[as.numeric(as.character(cut(as.numeric(fylke_map@data$result), q, include.lowest = TRUE, labels = c(1:6))))]
## plot(fylke_map, col = fylke_map@data$colour)

## Vilomraade we need a shapefile or some kind of spatial data
## viltreinomraade <- readHTMLTable("http://www.hjorteviltregisteret.no/HelseInnsyn/StatistikkCWD", which = 2, stringsAsFactors = FALSE)
