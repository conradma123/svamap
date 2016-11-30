library(svamap)
##
## Test 1 - Check for the data readfunction and succeful removal of postives
##
pts <- read_point_data()
pts <- pts[pts@data$Status..numerisk. == 0,]
stopifnot(all(pts@data$Status..numerisk. == 0))
rm(list = ls())
##
## Test 2 - Check for the warning for data without coordinates
##
temp <- read.csv2(system.file("sample_data_cwd.csv", package = "svamap"))
temp$Gisx[2] <- NA
path <- tempfile()
write.csv2(temp, file = path)
res <- tools::assertWarning(
    pts <- read_point_data(path))
stopifnot(length(grep("1 of the submitted points are missing coordinates",
                     res[[1]]$message)) > 0)
rm(list = ls())
##
## Test 3 - The static map
##
pts <- svamap::read_point_data(output_proj = "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0 +units=m +no_defs")
point_map(pts = pts, basemap = "lan", cex = 0.7)
##
##Test4 
pts <- read_point_data()
pts <- pts[(pts@data$Status..numerisk. == 0 &
            (pts@data$Publicera != "Nej" | is.na(pts@data$Publicera))
           ) |
           (pts@data$Publicera == "Ja" & !is.na(pts@data$Publicera)),]
stopifnot(identical(pts@data$Status..numerisk., c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L)))
rm(list = ls())
##
## Test 5 -  A table by län
##
pts <- read_point_data()
data(NUTS_20M)
polys <- svamap::match_to_county(pts, NUTS_20M, "NUTS_ID")
polys$polygons@data$count[is.na(polys$polygons@data$count)] <- 0
polys <- polys$polygons
df <- polys@data[,c("name", "count")]
df$count <- as.integer(df$count)
## write the table
tab <- html_table(df, col.names = c("Län", "Provtagna djur"))
tab <- html_table(df, html_head = generate_header(paging = TRUE), col.names = c("Län", "Provtagna djur"))
tab <- html_table(df, html_head = generate_header(ordering = TRUE), col.names = c("Län", "Provtagna djur"))
tab <- html_table(df, html_head = generate_header(info = TRUE), col.names = c("Län", "Provtagna djur"))
tab <- html_table(df, html_head = generate_header(searching = TRUE), col.names = c("Län", "Provtagna djur"))
tab <- html_table(df,
                  html_head = generate_header(paging = TRUE,
                                              ordering = TRUE,
                                              info = TRUE,
                                              searching = TRUE),
                  col.names = c("Län", "Provtagna djur")
                  )
rm(list = ls())
## write the table with do_Table
pts <- read_point_data()
data(NUTS_20M)
polys <- svamap::match_to_county(pts, NUTS_20M, "NUTS_ID")
polys$polygons@data$count[is.na(polys$polygons@data$count)] <- 0
polys <- polys$polygons
df <- polys@data[,c("name", "count")]
df$count <- as.integer(df$count)
table_do <- do_Table(df, lengthpage = 21)
rm(list = ls())
## make the map with choropleaf_hmap
pts <- read_point_data()
data(NUTS_20M)
polys <- svamap::match_to_county(pts, NUTS_20M, "NUTS_ID")
polys <- polys[[1]][1:4,]
polys@data$count <- c(1,2,3,4)
choropleaf_map(mapdata = polys,
               values = polys@data$count,
               palette = c("#FED98E", "#FE9929", "#CC4C02", "#FFFFD4"),
               labels = as.character(polys@data$count))
rm(list = ls())
