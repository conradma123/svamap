library(sp)
library(rgdal)

# Load kommuner from svamap package and change encoding --> peraphs to fix
load(file = system.file("data/kommuner.rda", package = "svamap"))
Encoding(kommuner@data$KnNamn) <- "UTF-8"

# Load postnummer data from svar package
load(file = system.file("data/postnummer2015.rda", package = "svar"))
postnummer2015$POSTALCODE <- as.character(postnummer2015$POSTALCODE) 

# Load kanin data. Change path from /media/t/ to T:/ to work locally
kanin <- read.csv2(file = "T:/Falkenrapporter/E16-045 Grundrapport.csv",
                    header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8",
                    na.strings = c("NA", " ", ""))

# Select just interesting columns
kanin_data_map <- data.frame(uppdrag = kanin$Uppdragid,
                             status = kanin$Status..numerisk.,
                             postort = kanin$Kundort,
                             postnum = kanin$Kundpostnr,
                             y = kanin$Gisx,
                             x = kanin$Gisy,
                             date = kanin$Ankomstdatum,
                             stringsAsFactors = FALSE)

kanin_data_map$date <- as.Date(kanin_data_map$date)

# Subset påvisad only samples after 1 January 2016
kanin_data_map <- kanin_data_map[kanin_data_map$date > "2016-01-01" &
                                 kanin_data_map$status == 1, ]

# Remove duplicated uppdrag
kanin_data_map <- kanin_data_map[!duplicated(kanin_data_map$uppdrag), ]
kanin_data_map$postnum <- sub(" ", "", kanin_data_map$postnum)

# Divide the dataset. Data with coordinates and data without coordinates
kanin_xy <- kanin_data_map[!is.na(kanin_data_map$x | kanin_data_map$y), ]
kanin_postnum <- kanin_data_map[is.na(kanin_data_map$x | kanin_data_map$y), ]

# Convert data with coordinates to a spatial object
coordinates(kanin_xy) <- cbind(kanin_xy$x, kanin_xy$y)
proj4string(kanin_xy) <- CRS("+init=epsg:3021")
kanin_xy <- spTransform(kanin_xy, CRSobj = proj4string(kommuner))

# Coordinates matching kommun
kanin_xy <- match_to_county(kanin_xy, kommuner, "KnNamn")
index <- kanin_xy[[1]]@data$KnNamn[!is.na(kanin_xy[[1]]@data$count) & kanin_xy[[1]]@data$count > 0]
kanin_xy <- kommuner[match(index,kommuner$KnNamn),]

if(!all(kanin_data_map$postnum %in% postnummer2015@data$POSTALCODE)) {
  warning("The following postnummer is/are not in our postnummer database: ", paste0(
    unique(kanin_data_map$postnum[!kanin_data_map$postnum %in% postnummer2015@data$POSTALCODE]),
    collapse = ", "), ".\n If not fixed, they will not be displayed in the final map" )
}

# Match the postnummer and then assign each postnummer to one kommun
kanin_postnum <- postnummer2015[postnummer2015@data$POSTALCODE %in% kanin_postnum$postnum,]
kanin_postnum <- kommuner[kommuner@data$KnKod %in% kanin_postnum$MUNICIPALI,]

# Remove duplicated kommun already included in kanin_xy
if(any(kanin_postnum$KnKod %in% kanin_xy$KnKod)) {
    kanin_postnum <- kanin_postnum[!kanin_postnum$KnKod %in% kanin_xy$KnKod, ]
}

kanin_final <- rbind(kanin_xy, kanin_postnum)

# Create a column of 1 to pass to the argument values in the function choropleaf_map
kanin_final@data$resultat <- 1

# Kvarka map. Change path from "/media/ESS_webpages/" to "//webutv/ESS/" to work locally
choropleaf_map (kanin_final,
                values = kanin_final@data$resultat,
                title = "Kaningulsot påvisad vid",
                palette = "red",
                labels = "minst 1 tilfälle under de senaste 12 månader",
                popup = kanin_final@data$KnNamn,
                logo = "/media/ESS_webpages/graphs/SVA_logo.png",
                src = "local",
                url = "http://www.sva.se/djurhalsa/andra-djurslag/kanin-sjukdomar/kaningulsot",
                disease = "kaningulsot",
                dir = "/media/ESS_webpages/kaningulsot",
                browse = FALSE)
