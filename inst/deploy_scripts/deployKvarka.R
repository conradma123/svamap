library(svamap)
library(sp)
library(rgdal)
library(RCurl)
##
## Load kommuner from svamap package and change encoding --> peraphs to fix
load(file = system.file("data/kommuner.rda", package = "svamap"))
Encoding(kommuner@data$KnNamn) <- "UTF-8"
##
## Load postnummer data from svar package
load(file = system.file("data/postnummer2015.rda", package = "svar"))
postnummer2015$POSTALCODE <- as.character(postnummer2015$POSTALCODE)
##
## Load kvarka data. Change path from /media/t/ to T:/ to work locally
kvarka <- read.csv2(file = "/media/t/Falkenrapporter/E15-026 Grundrapport.csv",
                    header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8",
                    na.strings = c("NA", " ", ""))
##
## Select just interesting columns
kvarka_data_map <- data.frame(uppdrag = kvarka$Uppdragid,
                              status = kvarka$Status..numerisk.,
                              postort = kvarka$Kundort,
                              postnum = kvarka$Kundpostnr,
                              date = kvarka$Ankomstdatum,
                              stringsAsFactors = FALSE)
##
kvarka_data_map$date <- as.Date(kvarka_data_map$date)
##
## Subset påvisad only samples
kvarka_data_map <- kvarka_data_map[kvarka_data_map$status == 1 &
                                     !is.na(kvarka_data_map$status), ]
##
## Subset samples in the period and create a new column 2 years - last 60 days (2) and last 60 days (1)
kvarka_data_map$period <- ifelse(kvarka_data_map$date >= Sys.Date()- 730 & kvarka_data_map$date < Sys.Date()- 60, 2,
                          ifelse(kvarka_data_map$date >= Sys.Date()- 60, 1, 0))
##
## Be sure to exclude records older than two years
kvarka_data_map <- kvarka_data_map[kvarka_data_map$period != 0, ]
##
## Remove duplicated uppdrag
kvarka_data_map <- kvarka_data_map[!duplicated(kvarka_data_map$uppdrag), ]
##
kvarka_data_map$postnum <- sub(" ", "", kvarka_data_map$postnum)
##
if(!all(kvarka_data_map$postnum %in% postnummer2015@data$POSTALCODE)) {
  warning("The following postnummer is/are not in our postnummer database: ", paste0(
                 unique(kvarka_data_map$postnum[!kvarka_data_map$postnum %in% postnummer2015@data$POSTALCODE]),
                 collapse = ", "), ".\n If not manually assigned to a kommun, they will not be displayed in the final map" )
}
##
## Assign each postnummer to a kommun
kvarka_data_map$kommun <- postnummer2015$MUNICIPALI[match(kvarka_data_map$postnum, postnummer2015@data$POSTALCODE)]
##
## Manual fix of those records not present in our postnummer dataset.
kvarka_data_map$kommun[kvarka_data_map$postnum == 75007 | kvarka_data_map$postnum == 75189] <- "0380" # UPPSALA (SVA,SLU)
kvarka_data_map$kommun[kvarka_data_map$postnum == 12922] <- "0180" # STOCKHOLM
kvarka_data_map$kommun[kvarka_data_map$postnum == 25023] <- "1283" # HELSNINGBORG
kvarka_data_map$kommun[kvarka_data_map$postnum == 20120] <- "1280" # MALMÖ
##
## Drop records not present in the postnummer database and not yet fixed manually
kvarka_data_map <- kvarka_data_map[!is.na(kvarka_data_map$kommun),]
##
if(!all(nchar(kvarka_data_map$kommun) == 4)) {
  warning("The following postnummer encompass more than one kommun: ", paste0(
    unique(kvarka_data_map$postnum[nchar(kvarka_data_map$kommun) != 4]),
    collapse = ", "), ".\n They will be assigned to the nearest kommun" )
}
##
#################################################################
#FIX OF THE PROBLEM OF POSTNUMMERS COVERING MORE THAN ONE KOMMUN#
#################################################################
##
## List of postnummer with more than one kommun
kvarka_more_kom <- kvarka_data_map[nchar(kvarka_data_map$kommun) != 4, ]
post_more_kom <- postnummer2015[match(kvarka_more_kom$postnum, postnummer2015@data$POSTALCODE),]
##
## Centroid of postnummer with more than one kommun
centroids <- data.frame(ID = rownames(coordinates(post_more_kom)),
                        x = coordinates(post_more_kom)[,1],
                        y = coordinates(post_more_kom)[,2],
                        postnum = post_more_kom@data$POSTALCODE,
                        posort = post_more_kom@data$LOCALITY,
                        KnKod = post_more_kom@data$MUNICIPALI,
                        stringsAsFactors = FALSE)
##
coordinates(centroids) <- cbind("x", "y")
proj4string(centroids) <- proj4string(post_more_kom)
##
## change to proj4string to UTM
kommuner_utm <- spTransform(kommuner, proj4string(post_more_kom))
##
## Find index of the nearest kommun
min_dist = apply(spDists(centroids, kommuner_utm), 1, which.min)
##
## Filter the KnKod of the kommun with min dist
kommun_min_dist <- kommuner_utm$KnKod[min_dist]
##
## Assign a unique kommun
kvarka_more_kom$kommun <- kommun_min_dist
##
## Drop records not reffering to a unique kommun
kvarka_data_map$kommun[nchar(kvarka_data_map$kommun) != 4] <- kvarka_more_kom$kommun
##
####### COMMENT ####################################################################
####### NEAREST NEIGHBOR DO NOT ALWAYS WORK. See postnummer 78173 & 79193.
####### Borlange period (1) cover Falun (period 2). The nearest neighboor assign
####### always to Borlange. Both are in kommun "2081" and so FALUN is not showed.
##      kvarka_data_map[order(kvarka_data_map$kommun),]
####### On the other hand it solves the bias of Hedemora (KnKod == "2083").
####### This kommun is not present in the dataset but displayed in ArcgisOnline map
##      grep("2083", kvarka_data_map$kommun)
####################################################################################
##
## Count number of occurrences per kommun and period.
pavisad_final <- as.data.frame(table(kvarka_data_map$kommun, kvarka_data_map$period), stringsAsFactors = FALSE)
colnames(pavisad_final) <- c("kommun", "period", "count")
##
## Create dataframes to be used later on to count occurrences in the popup
popup1 <- pavisad_final[pavisad_final$period ==  1,]
popup2 <- pavisad_final[pavisad_final$period ==  2,]
##
## Discharge those that have 0 occurrences
pavisad_final <- pavisad_final[pavisad_final$count != 0, ]
##
## Create a kolumn with the expected values to display in the choropleth map
pavisad_final$resultat <- ifelse(pavisad_final$period == 1 & pavisad_final$count == 1, 1,
                          ifelse(pavisad_final$period == 1 & pavisad_final$count == 2, 2,
                          ifelse(pavisad_final$period == 1 & pavisad_final$count >= 3, 3,
                          ifelse(pavisad_final$period == 2 & pavisad_final$count >= 1, 4, 0))))
##
## Delete duplicates. Note that the function drops always the second duplicated record.
pavisad_final <- pavisad_final[order(pavisad_final$period), ]
pavisad_final <- pavisad_final[!duplicated(pavisad_final$kommun), ]
##
## Create a spdf with just those kommuner with påvisad samples
kvarka_map <- kommuner[which(kommuner@data$KnKod %in% pavisad_final$kommun), ]
kvarka_map@data <- merge(kvarka_map@data, pavisad_final, by.x = "KnKod", by.y = "kommun")
##
## Change spdf reference system
kvarka_map <- spTransform(kvarka_map, CRS("+proj=longlat +datum=WGS84 +no_defs"))
##
## Create a string with the popup text to use in the map
popup_60d <- popup1$count[match(kvarka_map@data$KnKod, popup1$kommun)]
popup_24m <- popup2$count[match(kvarka_map@data$KnKod, popup2$kommun)]
##
kvarka_map@data$popup_text <-    paste("<b>Kommun:</b>", kvarka_map@data$KnNamn, "<br/>",
                                       "<b>Antal påvisade kvarkaprover</b>", "<br/>",
                                       " - senaste 2 månaderna: ", popup_60d,"<br/>",
                                       " - senaste 3-24 månaderna: ", popup_24m)
##
## Table of number of kommun per resultat
##
mylabel <- c("Ett tillfälle under de senaste 2 månaderna",
            "Två tillfällen under de senaste 2 månaderna",
            "Tre eller fler tillfällen under de senaste 2 månaderna",
            "Minst 1 tillfälle under senaste 3-24 månaderna")
kvarka_map@data$resultat <- factor(kvarka_map@data$resultat,
                                   levels = c(1, 2, 3, 4),
                                   labels = mylabel)
table_kvarka <- table(kvarka_map@data$resultat)
kvarka_map@data$resultat <- as.numeric(kvarka_map@data$resultat)
##
table_kvarka <- data.frame(mylabel = mylabel, kommuner = as.integer(table_kvarka))
path_to_data <- write_data(list(kvarka_map))
##
##Deploy map to internal server
########################
svamap::write_page(data = path_to_data,
                   path = "/media/ESS_webpages/kvarka/",
                   template = "kvarka/map.html",
                   overwrite = TRUE,
                   browse = FALSE)
##
## Deploy map to Azure server. This is SVA's external website and is
## administered by a company "Episerver hosting" the contact for this
## company at SVA is the communications department.
temp <- readLines("~/.svaftp_credentials")
cred <- paste0("ftp://", temp[2], ":", temp[3], "@", temp[1], "/MAPS/kvarka/")
svamap::write_page(data = path_to_data,
                   template = "kvarka/map.html",
                   overwrite = TRUE,
                   browse = FALSE,
                   ftp = cred)
## Kvarka table
## Add colours to the rows
colours_css <- c("<style>",
"  /**/",
"  /*First reset the style of the td element and then color the rows*/",
"  /**/",
"  .svatablegrayheader th {",
"  font-weight: bold;",
"  }",
"  .svatablegrayheader td {",
"  background-color: transparent;",
"  }",
"  .svatablegrayheader tbody tr:nth-child(1)",
"  {",
"  background-color: #FED98E;",
"  }",
"  .svatablegrayheader tbody tr:nth-child(2)",
"  {",
"    background-color: #FE9929;",
"  }",
"  .svatablegrayheader tbody tr:nth-child(3)",
"  {",
"  background-color: #CC4C02;",
"  }",
"  .svatablegrayheader tbody tr:nth-child(4)",
"    {",
"  background-color: #FFFFD4;",
"  }",
"</style>")
tab <- html_table(table_kvarka,
                  align = c("l", "r"),
                  col.names = c("Antal påvisade kvarkaprover", "Antal kommuner"),
                  html_head = generate_header(otherstuff = colours_css,
                                              ordering =FALSE),
                  footer = FALSE
                  )
temp <- readLines("~/.svaftp_credentials")
cred <- paste0("ftp://", temp[2], ":", temp[3], "@", temp[1], "/MAPS/Kvarka_table/")
ftpUpload(tab, paste0(cred, "kvarka_table.html"))
