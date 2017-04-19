library(svamap)
library(sp)
library(git2r)
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
stopifnot(all(pts@data$Aiv313 %in% c("ej påvisad", "PÅVISAD", "")))
stopifnot(all(pts@data$Aivh5313 %in% c("", "PÅVISAD", "ej påvisad")))
stopifnot(all(pts@data$Aivh7313 %in% c("", "ej påvisad")))
pts@data$result <- ifelse(pts@data$Aiv313 == "PÅVISAD", 1,
                   ifelse(pts@data$Aiv313 == "ej påvisad", 0, 2))
pts@data$result <- as.integer(pts@data$result)
pts <- pts[pts@data$Ankomstdatum > "2016/01/01 00:00:00" & pts@data$Ankomstdatum < "2017/01/01 00:00:00", ]
##drop duplicates
pts <- pts[!duplicated(pts@data$Namn), ]
## drop those positive on 'Matrix' but are negative for H5 and H7
pts <- pts[!(pts@data$result == 1 & !(pts@data$Aivh5313 == "PÅVISAD" | pts@data$Aivh7313 == "PÅVISAD")),]
pts@data$Namn[pts@data$Namn == ""] <- pts@data$Ringmärkning[pts@data$Namn == ""]
##View(pts@data[order(pts@data$Ankomstdatum),])
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
approved <- c("VLT 2259/16", "VLT 2278/16", "VLT 2248/16",
              "VLT 2261/16", "VLT 2163/16", "VLT 2260/16",
              "VLT 2277/16",
              "VLT 2347/16",
              "VLT 2356/16",
              "VLT 2363/16",
              "VLT 2364/16",
              "VLT 2365/16",
              "VLT 2430/16",
              "VLT 2421/16",
              "VLT 2423/16",
              "VLT 2488/16",
              "VLT 2436/16",
              "VLT 23/17",
              "VLT 46/17",
              "VLT 104/16, 2",
              "VLT 105/17, 1",
              "VLT 138/17",
              "VLT 216/17",
              "VLT 283/17",
              "VLT 298/17",
              "VLT 300/17",
              "VLT 284/17",
              "VLT 286/17",
              "VLT 285/17",
              "VLT 304/17",
              "VLT 287/17",
              "VLT 289/17",
              "VLT 290/17",
              "VLT 314/17",
              "VLT 321/17",
              "VLT 312/17",
              "VLT 313/17",
              "VLT 901/17",
              "VLT 902/17",
              "VLT 889/17",
              "VLT 887/17",
              "VLT 970/17",
              "VLT 973/17",
              "VLT 975/17",
              "VLT 974/17",
              "VLT 976/17",
              "VLT 64/17",
              "VLT 924/17",
              "VLT 923/17",
              "VLT 1004/17",
              "VLT 1105/17",
              "VLT 191/17, Accnr. 20176010",
              "VLT 1047/17",
              "VLT 1175/17",
              "VLT 1266/17",
              "VLT 1278/17",
              "VLT 2356/16, Acc nr: 20167013.")
pts@data$highpath <- pts@data$result == 0 | pts@data$ViltID %in% approved
pts@data <- subset(pts@data, select = -c(ViltID))
pts@data$location <- as.numeric(as.factor(paste0(coordinates(pts)[, 1], coordinates(pts)[, 2])))
## Deal with points that land in exactly the same position, deal with
## overlapping points separately in the positives and negatives. This
## is done so that the positives and negatives are always plotted
## separately and that the positives are always plotting on top of the
## negatives.
pos <- pts[pts$result == 1 & pts@data$highpath,]
pos_uncertain <- pts[pts$result == 1 & !pts@data$highpath,]
neg <- pts[pts$result == 0,]
neg_un <- do.call("rbind", lapply(unique(neg@data$location), function(x){
    neg[neg@data$location == x,][1,]
}))
for(i in neg_un@data$location){
    temp <- as.data.frame(table(neg[neg@data$location == i,]$Ankomstdatum,
                                neg[neg@data$location == i,]$species)
                          )
    names(temp) <- c("Datum", "Art", "Antal")
    temp <- temp[temp$Antal != 0,]
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
    temp <- temp[temp$Antal != 0,]
    pos_un@data$popup_text[pos_un@data$location == i] <- paste0(html_table(temp, fragment = TRUE, file = NULL), collapse = "\n")
    pos_un@data$n[pos_un@data$location == i] <- sum(temp$Antal)
}
pos_uncertain@data$popup_text <- ""
pos_uncertain@data$n <- 1
##Order the positive second so they get plotted last
pts <- rbind(neg_un, pos_uncertain, pos_un)
##Calculate the radius of the point
pts@data$radius <- round((pts@data$n*50/3.1415)^0.5, 1)
pts@data <- subset(pts@data, select = c(result, popup_text, radius, Ankomstdatum, n, highpath ))
pts@data$result[pts@data$result == 1 & !pts@data$highpath] <- 2
##
library(sp)
load("/home/trosendal/projects/small_projects/svamap/svamap/data/water.Rda")
load("/home/trosendal/projects/small_projects/svamap/svamap/data/fancylan.Rda")
fancylan <- spTransform(fancylan, "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
water <- spTransform(water, "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
pts <- spTransform(pts, "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
pts <- pts[pts@data$result != 0,]
pts@data$colour <-c("#D22630", "#F2A900")[pts@data$result]
pdf("~/Desktop/influenza.pdf", height = 15, width = 7)
plot(water, add = FALSE, col = "#a7cdf2", border = "#a7cdf2")
plot(fancylan, add = TRUE, border = "grey40")
plot(pts, add = TRUE, pch = 21, cex = pts@data$radius/3, bg=pts@data$colour, col = "grey20")
legend(222235, 7258747, legend = c("HPAI", "Some text?"),
       pch = 21, pt.cex = 2, pt.bg = c("#D22630", "#F2A900"), col = "grey20",
       cex = 1.5, bty = "n")
dev.off()
