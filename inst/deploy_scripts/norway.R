library(svamap)
## Read data from website
library(XML)
## Read the kommune data:
df <- readHTMLTable("http://www.hjorteviltregisteret.no/HelseInnsyn/StatistikkCWD", which = 1, stringsAsFactors = FALSE)
## Keep just kommune
kommune <- df[trimws(df$Fylke) == "",]
## Get boundaries
data(kommuner_norge, package = "svamap")
## Define a colour scale
colours <- c('#f7f7f7','#d9d9d9','#bdbdbd','#969696','#636363','#252525')
## match the data
kommuner_norge@data$result <- kommune$Totalt[match(kommuner_norge@data$navn, kommune$Kommune)]
kommuner_norge@data$positive <- kommune$Positive[match(kommuner_norge@data$navn, kommune$Kommune)]
## Assign colours by counts
q <- quantile(as.numeric(kommuner_norge@data$result),
              na.rm = TRUE,
              probs = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1))
kommuner_norge@data$colour <- colours[as.numeric(as.character(cut(as.numeric(kommuner_norge@data$result),
                                                               q,
                                                               include.lowest = TRUE, labels = c(1:6))
                                                           )
                                              )]
kommuner_norge$popup_text <- paste0(kommuner_norge$navn,
                                 "<br>Total number tested: ", kommuner_norge@data$result,
                                 "<br>Total number Positive: ", kommuner_norge@data$positive)
## make positive kommuner red
kommuner_norge@data$colour[which(as.numeric(kommuner_norge@data$positive)>0)] <- "#D22630"
## Create popup text
kommuner_norge@data <- kommuner_norge@data[,names(kommuner_norge@data) %in% c("navn", "result", "colour", "popup_text")]
##
## Read the Villreinområde data:
df <- readHTMLTable("http://www.hjorteviltregisteret.no/HelseInnsyn/StatistikkCWD", which = 2, stringsAsFactors = FALSE)
names(df)[1] <- "name"
vro <- df
## Get boundaries:
data(villreinomrader, package = "svamap")
## match the data (There are 4 areas in the web data not represented
## in the map data) Also forced to use a fuzzy match because the names
## are alittle different
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
q <- quantile(as.numeric(Villreinomrader_norge@data$result),
              na.rm = TRUE,
              probs = c(0, 0.7, 0.75, 0.8, 0.85, 0.9,1))
Villreinomrader_norge@data$colour <- colours[as.numeric(as.character(cut(as.numeric(Villreinomrader_norge@data$result),
                                                               q,
                                                               include.lowest = TRUE, labels = c(1:6))
                                                           )
                                                        )]
Villreinomrader_norge@data$result[is.na(Villreinomrader_norge@data$result)] <- 0
Villreinomrader_norge@data$positive[is.na(Villreinomrader_norge@data$positive)] <- 0

Villreinomrader_norge$popup_text <- paste0(Villreinomrader_norge$Omradenavn,
                                 "<br>Total number tested: ", Villreinomrader_norge@data$result,
                                 "<br>Total number Positive: ", Villreinomrader_norge@data$positive)
## make positive kommuner red
Villreinomrader_norge@data$colour[which(as.numeric(Villreinomrader_norge@data$positive)>0)] <- "#D22630"
## Create popup text
Villreinomrader_norge@data <- Villreinomrader_norge@data[,names(Villreinomrader_norge@data) %in% c("Omradenavn", "result", "colour", "popup_text")]
##
## plot map
path_to_data <- svamap::write_data(list(kommuner_norge, Villreinomrader_norge))
svamap::write_page(data = path_to_data,
##                  path = "/media/ESS_webpages/Norway_CWD/",
                   owntemplate = "/home/trosendal/projects/small_projects/svamap/svamap/inst/Norway/map.html",
                   overwrite = TRUE,
                   browse = TRUE)
