library(svamap)
## Read data from website
library(XML)
##
##
## Read the kommune data in Norway:
##
##
df <- readHTMLTable("http://www.hjorteviltregisteret.no/HelseInnsyn/StatistikkCWD", which = 1, stringsAsFactors = FALSE)
## Keep just kommune
kommune <- df[trimws(df$Fylke) == "",]
## Get boundaries
data(kommuner_norge, package = "svamap")
## Define a colour scale
colours <- c('#f7f7f7','#d9d9d9','#bdbdbd','#969696','#636363','#252525')
## match the data:
## The Kommune "Ølen" and "Ramnes" are both represented
## in the web table but are not in the spatial data because they are
## no longer Kommuner in Norway.
kommuner_norge@data$result <- kommune$Totalt[match(kommuner_norge@data$navn, kommune$Kommune)]
kommuner_norge@data$positive <- kommune$Positive[match(kommuner_norge@data$navn, kommune$Kommune)]
## Assign colours by counts
## q <- quantile(as.numeric(kommuner_norge@data$result),
##               na.rm = TRUE,
##               probs = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1))
q <- c(0, 3, 10, 20, 50, 100, max(as.numeric(kommuner_norge@data$result), na.rm = TRUE))
kommuner_norge@data$colour <- colours[as.numeric(as.character(cut(as.numeric(kommuner_norge@data$result),
                                                               q,
                                                               include.lowest = TRUE, labels = c(1:6))
                                                           )
                                                 )]
kommuner_norge@data$result[is.na(kommuner_norge@data$result)] <- 0
kommuner_norge@data$positive[is.na(kommuner_norge@data$positive)] <- 0
kommuner_norge@data$colour[kommuner_norge@data$result == 0] <- colours[1]
kommuner_norge$popup_text <- paste0("Kommune: ", kommuner_norge$navn,
                                 "<br>Totalt antal: ", kommuner_norge@data$result,
                                 "<br>Antal positiva: ", kommuner_norge@data$positive)
## make positive kommuner red
kommuner_norge@data$colour[which(as.numeric(kommuner_norge@data$positive)>0)] <- "#D22630"
## Create popup text
kommuner_norge@data <- kommuner_norge@data[,names(kommuner_norge@data) %in% c("navn", "result", "colour", "popup_text")]
##
##
##
## Read the Villreinområde data:
##
##
##
df <- readHTMLTable("http://www.hjorteviltregisteret.no/HelseInnsyn/StatistikkCWD", which = 2, stringsAsFactors = FALSE)
names(df)[1] <- "name"
vro <- df
## Get boundaries:
data(villreinomrader, package = "svamap")
## Change some names to match the boundary data
Villreinomrader_norge@data$Omradenavn[Villreinomrader_norge@data$Omradenavn == "Ottadalsområdet"] <- "Reinheimen-Breheimen"
Villreinomrader_norge@data$Omradenavn[Villreinomrader_norge@data$Omradenavn == "Snøhettaområdet"] <- "Snøhetta"
Villreinomrader_norge@data$Omradenavn[Villreinomrader_norge@data$Omradenavn == "Våmur/Roan"] <- "Våmur Roan"
Villreinomrader_norge@data$Omradenavn[Villreinomrader_norge@data$Omradenavn == "Skaulen/Etnefjella"] <- "Skaulen Etnefjella"
## match the data. There are 3 areas in the web data not represented
## in the map data (Semmeldalen, Colesdalen, Reindalen) Also forced to
## use a fuzzy match because the names are a little different
index <- do.call("c",lapply(Villreinomrader_norge@data$Omradenavn, function(x) {
    if(length(agrep(x, vro$name, max.distance = 0.1))) {
        agrep(x, vro$name, max.distance = 0.1)
    } else {
        return(NA)
    }
}))
Villreinomrader_norge@data$result <- vro$Totalt[index]
Villreinomrader_norge@data$positive <- vro$Positive[index]
## Assign colours by counts
## q <- quantile(as.numeric(Villreinomrader_norge@data$result),
##               na.rm = TRUE,
##               probs = c(0, 0.7, 0.75, 0.8, 0.85, 0.9,1))
q <- c(0, 3, 10, 20, 50, 100, max(as.numeric(Villreinomrader_norge@data$result), na.rm = TRUE))
Villreinomrader_norge@data$colour <- colours[as.numeric(as.character(cut(as.numeric(Villreinomrader_norge@data$result),
                                                               q,
                                                               include.lowest = TRUE, labels = c(1:6))
                                                           )
                                                        )]
Villreinomrader_norge@data$result[is.na(Villreinomrader_norge@data$result)] <- 0
Villreinomrader_norge@data$positive[is.na(Villreinomrader_norge@data$positive)] <- 0
Villreinomrader_norge@data$colour[Villreinomrader_norge@data$result == 0] <- colours[1]
Villreinomrader_norge$popup_text <- paste0("Villreinområde: ", Villreinomrader_norge$Omradenavn,
                                 "<br>Totalt antal: ", Villreinomrader_norge@data$result,
                                 "<br>Antal positiva: ", Villreinomrader_norge@data$positive)
## make positive kommuner red
Villreinomrader_norge@data$colour[which(as.numeric(Villreinomrader_norge@data$positive)>0)] <- "#D22630"
## Create popup text
Villreinomrader_norge@data <- Villreinomrader_norge@data[,names(Villreinomrader_norge@data) %in% c("Omradenavn", "result", "colour", "popup_text")]
##
##
## Add the Swedish sampling points to the map
##
##
data(NUTS_20M)
##
##Read in the point data
########################
pts <- read_point_data("/media/t/Falkenrapporter/E16-036 Grundrapport.csv")
########################################################
## REMOVE ALL POSITIVES if Publicera flagg isn't "Ja"###
########################################################
##
## Keep only values "Ja" and "Nej"
pts@data$Publicera <- factor(pts@data$Publicera, levels = c("Ja", "Nej"))
##
##Now keep all negatives unless Publicera is "Nej"; Drop all Positives unless Publicera is "Ja"
##
pts <- pts[(pts@data$Status..numerisk. == 0 &
            (pts@data$Publicera != "Nej" | is.na(pts@data$Publicera))
           ) |
           (pts@data$Publicera == "Ja" & !is.na(pts@data$Publicera)),]
##pts <- read_point_data()
##
## Drop the points that are not "Vilt (Jakt - fiske - natur)"
########################
pts <- pts[pts$Djurhållning == "Vilt (Jakt - fiske - natur)" & !is.na(pts$Djurhållning),]
##
## Define the text to go in the popup for the point data
########################
pts$popup_text <- paste0(pts$Djurslag,
                         "<br>Ankomstdatum: ", as.Date(pts$Ankomstdatum),
                         "<br>Djuridentitet: ", pts@data$Djuridentitet)
## Drop data that you don't want to display from points
########################
pts@data <- data.frame(species = pts@data$Djurslag,
                       result = pts@data$Status..numerisk.,
                       popup_text = pts@data$popup_text, stringsAsFactors = FALSE)
##
## plot map
path_to_data <- svamap::write_data(list(kommuner_norge, Villreinomrader_norge, pts))
svamap::write_page(data = path_to_data,
                   path = "/media/ESS_webpages/Norway_CWD/",
                   template = "Norway/map.html",
                   overwrite = TRUE,
                   browse = FALSE)
## Put is on an available place
temp <- readLines("~/.svaftp_credentials")
cred <- paste0("ftp://", temp[2], ":", temp[3], "@", temp[1], "/MAPS/CWD_with_norway/")
svamap::write_page(data = path_to_data,
                   template = "Norway/map.html",
                   overwrite = TRUE,
                   browse = FALSE,
                   ftp = cred)
