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

## More about data structure

The two examples above are quite specific. The range of requirements
for colour, titles, labels and many other map attributes is quite
broad. In order to produce a map of some sort the first step is to
decide approximately what you want to display and then start shaping
your data to make that possible. Once the data is in the
appropriate shape the development of the map itself in .html can begin
and will use this data as input. The goal for a choropleth map is to
produce
a
[SpatialPolygonsDataFrame](http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/cheatsheet.html) which
is a datastructure in R that is defines in
the [sp](https://cran.r-project.org/web/packages/sp/sp.pdf)
package. It essentially is a two part data structure, the first part
defines the polygons and the second attributes of those polygons in a
standard dataframe. The goal for a point map is  to
produce
a
[SpatialPointsDataFrame](http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/cheatsheet.html) which
is a datastructure in R that is defines in
the [sp](https://cran.r-project.org/web/packages/sp/sp.pdf)
package. It essentially is a two part data structure, the first part
defines the points and the second attributes of those points in a
standard dataframe.

In this process you need to consider the [projection](https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf) of your spatial
data and what data that you want to display in your map associated
with the points or polygons.

### Projection

Modification of spatial projection in R is handled in the `rgdal`
package and the definition of a projection is a text string called
`proj4str`. In Swedish data there are essentially three projections to
consider:

RT90 is the 'old' Swedish projection, however it is still widely used
and there is nothing wrong with using it unless you ask a GIS expert who
would like us to migrate away from this projection. The
`proj4str` to define RT90 is

```{r}
"+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0"
```

The projection that should replace RT90 is called SWEREF99 and is
defined as follows:

```{r}
"+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
```

Finally, the unprojected, latitude and longitude with spheroid WGS84
is the target projection for our data because it is the native format
for leaflet.js and is defined as follows:

```{r}
"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
```

### Data you want to display

The data you want to display or want to use to modify attributes of
your points or polygons are contained in a dataframe that is attached
to the Spatial data. Think about which things you need and included
nothing but the minimum necessary information in your data. For
example you might need a status of a point (1/0) or the count of the
number of cases in a polygon (0,1,2,3,4...) your might also want to
display a popup text associated with a point or polygon. This popup
text should be included in a variable names `popup_text` in the
dataframe and the rest is up to you.

### Example of the process for points:

You might start with some X, Y coordinates of points that you want to
map. You need to:

1. Produce a `SpatialPointsDataFrame` from the points
2. Define its projection
3. Reproject it to WGS84
4. Define the variables would want to have in your map
5. Discard anything in the dataframe that you don't want to share with
   your reader.

Some points in a dataframe:

```{r}

df <- structure(list(Y = c(7441530L, 6554248L, 6593885L, 6685965L,
                           6569576L, 6594293L, 6656500L, 6666097L,
                           6406668L, 6666097L, 6652659L, 6169384L,
                           6624004L, 6612043L),
                     X = c(1822295L, 1628507L, 1593520L, 1361045L,
                           1279962L, 1638184L, 1621171L, 1576149L,
                           1259189L, 1259189L, 1534008L, 1334547L,
                           1360313L, 1642178L),
                     Status = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0,
                                0),
                     secretid = 1:14),
                .Names = c("Y", "X", "Status", "secretid"),
                row.names = c(NA, -14L), class = "data.frame")

```

1. Now produce a SpatialPointsDataFrame from this data. If you want to
   know more about this a good resource
   is
   [here](http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/cheatsheet.html). Sometimes
   it can be a little tricky to know which is X and Y, you may find
   that you go through to whole process and plot your points only to
   find out that you swapped X and Y.

```{r}
library(sp)
spdf <- SpatialPointsDataFrame(cbind(df$X, df$Y), df)

## Have a look at the coordinates of the SpatialPointsDataFrame
spdf@coords
```

2. Now you have a SpatialPointsDataFrame and you need to declare its
   projection in order to subsequently reproject it to WGS84. In this
   case we also know that the projection of the data is RT90. You may
   also find that you need to research the origin of you data in order
   to get this correct.

```{r}
proj4string(spdf) <- "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0"
```

3. Now you can reproject (modify) the coordinates to WGS84. For this
   you need the `rgdal` package

```{r}
library(rgdal)
spdf <- spTransform(spdf, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## Notice that the coordinates have changed:
spdf@coords
```

4. Define the variables you want to include in your map:

```{r}

spdf@data$popup_text <- paste("This point has a status of", spdf@data$Status)

```

5. Drop the data you don't want to share:

```{r}
spdf@data <- spdf@data[,c("Status", "popup_text")]
```


#### Write your data to a format that can be used for an html map

You now have data ready for plotting a map and the last step is to
write this to .json format:

```{r}
myfile <- write_data(spdf, varname = "data")
readLines(myfile)
```

This will produce a .json formatted dataset defines in javascript with
the variable name "data1" and a timestamp of when the file was
produced. In order to map this you need to select an appropriate map
template that suits your data structure or more likely you need to
write a map template to suit the needs of the map you will publish.
