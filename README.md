[![Build Status](https://travis-ci.org/SVA-SE/svamap.svg?branch=master)](https://travis-ci.org/SVA-SE/svamap)


# svamap - leaflet maps from R datasets

The design of this package is that you read in your data to R and
produce a spatial dataset of that. The example dataset is a .csv file
in the inst folder that you can read in using `read_point_data()`.

Next you export the data to geojson and write it to a .js file that is
then submitted to a leaflet webpage.

We have deployed a few pages using the package:

* [A map of CWD testing in Sweden and Norway](http://www.sva.se/Maps/CWD_with_norway/map.html)

* [A dynamic time-filtering map of AI testing in Sweden](http://www.sva.se/Maps/AI_timeslider/map.html)

* [A species table of CWD testing in Sweden](http://www.sva.se/Maps/CWD_species_table/table.html)

* [A weather forecast map with lines and points](http://www.epi-cloud.org/vattern/830/map.html)

* [A map of strangles sampling at the county level in sweden](http://www.sva.se/smittlage/kvarkakarta)

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
## Write out the data to a geojson
path_to_data <- write_data(polys)
## Add the data to the leaflet webpage, in this case template "map"
svamap::write_page(path_to_data, template = "map", overwrite = TRUE, browse = TRUE)
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
## Write to geojson
path_to_data <- write_data(list(polys, pts))
## add the data to "map2" template in the package
svamap::write_page(path_to_data, path = "/tmp", template = "map2", overwrite = TRUE, browse = FALSE)
```

## Design

You might wonder why I am not using the Rleaflet library or similar
for this. It is about flexibility of designing the map in only html
and javascript and the only thing R does is format the data. The down
side is the you need to understand R, javascript and html to produce a
map.
