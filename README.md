[![Build Status](https://travis-ci.org/SVA-SE/svamap.svg?branch=master)](https://travis-ci.org/SVA-SE/svamap)
[![Build status](https://ci.appveyor.com/api/projects/status/t5tume57ympjqm3r?svg=true)](https://ci.appveyor.com/project/trosendal/svamap)


The latest build for Windows is available here:
[Windows build svamap\_0.3.zip](https://ci.appveyor.com/api/projects/trosendal/svamap/artifacts/svamap_0.3.zip)

Then install it like this:

```r
install.packages("PATH_TO_YOUR_DOWNLOADS/svamap_0.3.zip", repos = NULL)
```

# svamap - leaflet maps from R datasets

The design of this package is that you read in your data to R and
produce a spatial dataset of that. The example dataset is a .csv file
in the inst folder that you can read in using `read_point_data()`.

Next you export the data to geojson and write it to a .js file that is
then submitted to a leaflet webpage.

We have deployed a few pages using the package:

* <a target = "_blank" href = "http://www.sva.se/Maps/CWD_with_norway/map.html">A map of CWD testing in Sweden and Norway</a>

* <a target = "_blank" href = "http://www.sva.se/Maps/AI_timeslider/map.html">A dynamic time-filtering map of AI testing in Sweden</a>

* <a target = "_blank" href = "http://www.sva.se/Maps/CWD_species_table/table.html">A species table of CWD testing in Sweden</a>

* <a target = "_blank" href = "http://www.epi-cloud.org/vattern/830/map.html">A weather forecast map with lines and points</a>

* <a target = "_blank" href = "http://www.sva.se/smittlage/kvarkakarta">A map of strangles sampling at the county level in Sweden with a summary table</a>

## Two examples are given:

First a map with just a polygon layer that summarises the count of the
number of points in a polygon and binds it to a popup.

```{r eval = FALSE}
library(svamap)
##Get the polygon data
data(NUTS_20M)
##Get the point data
pts <- read_point_data()
## Match the location of the points to the polygons and add a count variable to the polygon dataset
polys <- svamap::match_to_county(pts, NUTS_20M, "NUTS_ID")
## just keep the points that match a polygon:
polys <- polys[[1]]
## make some popup text:
polys@data$popup_text <- as.character(polys@data$count)
## Write out the data to a geojson (Must be a character)
path_to_data <- write_data(polys)
## Add the data to the leaflet webpage, in this case template "choropleth_map"
svamap::write_page(path_to_data,
                   template = "choropleth_map/map.html",
                   overwrite = TRUE,
                   browse = TRUE)
```

[A Second map](https://sva-se.github.io/svamap/map.html) with both a point and polygon layer. Notice the you
submit the multiple layers as a list to the `write_data()` function

```{r eval = FALSE}
library(svamap)
##Get the polygon data
data(NUTS_20M)
##Get the point data
pts <- read_point_data()
## Remove the information from the points that you don't want to display
pts@data <- data.frame(pts@data$Djurslag, stringsAsFactors = FALSE)
## Match points to polygon
polys <- svamap::match_to_county(pts, NUTS_20M, "NUTS_ID")
## just keep the matching polygons
polys <- polys[[1]]
## Assign 0 counts to those that have NA counts
polys@data$count[is.na(polys@data$count)] <- 0
## Add popup text to the polygons
polys@data$popup_text <- paste0(polys@data$name, "<br>Count = ", polys@data$count)
## Add popup text to the points
pts@data$popup_text <- pts@data$pts.data.Djurslag
## Write to geojson
path_to_data <- write_data(list(polys, pts))
## add the data to "map2" template in the package
svamap::write_page(path_to_data,
                   template = "choropleth_and_points/map.html",
                   overwrite = TRUE,
                   browse = TRUE)
```

## Design

You might wonder why I am not using the Rleaflet library or similar
for this. It is about flexibility of designing the map in only html
and javascript and the only thing R does is format the data. The down
side is the you need to understand R, javascript and html to produce a
map.
