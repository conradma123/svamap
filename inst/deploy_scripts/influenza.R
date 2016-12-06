library(svamap)
library(sp)
##
data(NUTS_20M)
##
##Read in the point data
########################
pts <- read_point_data("/media/t/Falkenrapporter/AI vilda fåglar.csv")
pts@data$Ankomstdatum <- as.Date(pts@data$Ankomstdatum)
pts <- pts[!is.na(pts@data$Aiv313),]
pts@data$Aiv313[pts@data$Aiv313 == "ej påvisad, ej påvisad"] <- "ej påvisad"
## Make sure the unique values in the Aiv313 vector are what we expect
stopifnot(identical(sort(unique(pts@data$Aiv313)), sort(c("ej påvisad", "PÅVISAD"))))
stopifnot(identical(sort(unique(pts@data$Aivh5313)), sort(c("", "PÅVISAD", "ej påvisad"))))
stopifnot(identical(sort(unique(pts@data$Aivh7313)), sort(c("", "ej påvisad"))))
pts@data$result <- ifelse(pts@data$Aiv313 == "PÅVISAD", 1,
                   ifelse(pts@data$Aiv313 == "ej påvisad", 0, 2))
pts@data$result <- as.integer(pts@data$result)
pts <- pts[pts@data$Ankomstdatum > "2016/09/01 00:00:00", ]
##drop duplicates
pts <- pts[!duplicated(pts@data$Namn), ]
## drop those positive on 'Matrix' but are negative for H5 and H7
pts <- pts[!(pts@data$result == 1 & !(pts@data$Aivh5313 == "PÅVISAD" | pts@data$Aivh7313 == "PÅVISAD")),]
pts@data <- data.frame(species = pts@data$Djurslag,
                       result = pts@data$result,
                       ViltID = pts@data$Namn,
                       Ankomstdatum = pts@data$Ankomstdatum,
                       stringsAsFactors = FALSE)
## Drop if the sample is not yet complete:
pts <- pts[pts$result != 2,]
## sort by påvisade to get the positives plotted last
pts <- pts[order(pts@data$result),]
## Only keep those positives that have been approved to be published:
approved <- c("VLT 2261/16", "VLT 2259/16", "VLT 2248/16", "VLT 2163/16", "VLT 2260/16")
pts <- pts[pts@data$result == 0 | pts@data$ViltID %in% approved, ]
pts@data <- subset(pts@data, select = -c(ViltID))
pts@data$location <- as.numeric(as.factor(paste0(coordinates(pts)[, 1], coordinates(pts)[, 2])))
## Deal with points that land in exactly the same position, deal with
## overlapping points separately in the positives and negatives. This
## is done so that the positives and negatives are always plotted
## separately and that the positives are always plotting on top of the
## negatives.
pos <- pts[pts$result == 1,]
neg <- pts[pts$result == 0,]
neg_un <- do.call("rbind", lapply(unique(neg@data$location), function(x){
    neg[neg@data$location == x,][1,]
}))
for(i in neg_un@data$location){
    temp <- as.data.frame(table(neg[neg@data$location == i,]$Ankomstdatum,
                                neg[neg@data$location == i,]$species)
                          )
    names(temp) <- c("Datum", "Art", "Antal")
    neg_un@data$popup_text[neg_un@data$location == i] <- paste0(html_table(temp, fragment = TRUE, file = NULL), collapse = "\n")
    neg_un@data$n[neg_un@data$location == i] <- sum(temp$Antal)
}
pos_un <- do.call("rbind", lapply(unique(pos@data$location), function(x){
    pos[pos@data$location == x,][1,]
}))
for(i in pos_un@data$location){
    temp <- as.data.frame(table(pos[pos@data$location == i,]$Ankomstdatum,
                                pos[pos@data$location == i,]$species)
                          )
    names(temp) <- c("Datum", "Art", "Antal")
    pos_un@data$popup_text[pos_un@data$location == i] <- paste0(html_table(temp, fragment = TRUE, file = NULL), collapse = "\n")
    pos_un@data$n[pos_un@data$location == i] <- sum(temp$Antal)
}
##Order the positive second so they get plotted last
pts <- rbind(neg_un, pos_un)
##Calculate the radius of the point
pts@data$radius <- round((pts@data$n*50/3.1415)^0.5, 1)
pts@data <- subset(pts@data, select = c(result, popup_text, radius))
##Write data to geojson
########################
path_to_data <- svamap::write_data(pts)
########################
##Deploy map to internal server
########################
svamap::write_page(data = path_to_data,
                   path = "/media/ESS_webpages/AI/",
                   template = "influenza",
                   overwrite = TRUE,
                   browse = FALSE)
##
## Deploy map to Azure server. This is SVA's external website and is
## administered by a company "Episerver hosting" the contact for this
## company at SVA is the communications department.
temp <- readLines("~/.svaftp_credentials")
cred <- paste0("ftp://", temp[2], ":", temp[3], "@", temp[1], "/MAPS/AI/")
svamap::write_page(data = path_to_data,
                   template = "influenza",
                   overwrite = TRUE,
                   browse = FALSE,
                   ftp = cred)
