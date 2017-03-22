library(svamap)
library(rgeos)
library(sp)
library(RCurl)
##
data(NUTS_20M)
##
##Read in the point data
########################
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
########################
## Project to planar
org_proj <- proj4string(NUTS_20M)
counties <- spTransform(NUTS_20M, CRSobj = "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0 +units=m +no_defs")
## Put a buffer to catch the points on the perimeter: This is
## suboptimal since an overlapping internal boundaries are created in
## the gBuffer function
temp <- gBuffer(counties, byid = TRUE, width = 10000)
counties <- spTransform(counties, org_proj)
temp <- spTransform(temp, org_proj)
polys <- svamap::match_to_county(pts, counties, "NUTS_ID")
polys2 <- svamap::match_to_county(polys$outlier_pts, temp, "NUTS_ID")
polys$polygons@data$count[is.na(polys$polygons@data$count)] <- 0
polys2$polygons@data$count[is.na(polys2$polygons@data$count)] <- 0
## Add the perimeter points to the county count
polys$polygons@data$count <- polys$polygons@data$count + polys2$polygons@data$count
polys <- polys$polygons
## Just keep the basic info for the table
df <- polys@data[,c("name", "count")]
df$count <- as.integer(df$count)
total <- sum(df$count)
df <- rbind(df, c("Total", total))
## write the table
nolink <- c("<META NAME='ROBOTS' CONTENT='NOINDEX, NOFOLLOW'>")
tab <- html_table(df,
                  align = c("l", "r"),
                  col.names = c("Län", "Antal undersökta"),
                  html_head = generate_header(ordering =TRUE, otherstuff = nolink),
                  footer = TRUE
                  )
## Deploy map to Azure server. This is SVA's external website and is
## administered by a company "Episerver hosting" the contact for this
## company at SVA is the communications department.
temp <- readLines("~/.svaftp_credentials")
cred <- paste0("ftp://", temp[2], ":", temp[3], "@", temp[1], "/MAPS/CWD/")
ftpUpload(tab, paste0(cred, "table.html"))
