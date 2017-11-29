library(svamap)
library(sp)
library(git2r)
##
data(NUTS_20M)
##
##Read in the point data
########################
pts <- read.csv2("/media/t/Falkenrapporter/AI vilda fåglar.csv", stringsAsFactors = FALSE)
pts$Ankomstdatum <- as.Date(pts$Ankomstdatum)
pts <- pts[!is.na(pts$Aiv313),]
pts$Aiv313[pts$Aiv313 == "ej påvisad, ej påvisad"] <- "ej påvisad"
## Make sure the unique values in the Aiv313 vector are what we expect
stopifnot(all(pts$Aiv313 %in% c("ej påvisad", "PÅVISAD", "")))
stopifnot(all(pts$Aivh5313 %in% c("", "PÅVISAD", "ej påvisad")))
stopifnot(all(pts$Aivh7313 %in% c("", "ej påvisad")))
pts$result <- ifelse(pts$Aiv313 == "PÅVISAD", 1,
                   ifelse(pts$Aiv313 == "ej påvisad", 0, 2))
pts$result <- as.integer(pts$result)
pts <- pts[pts$Ankomstdatum > "2016/01/01 00:00:00", ]
##drop duplicates
pts <- pts[!duplicated(pts$Namn), ]
## drop those positive on 'Matrix' but are negative for H5 and H7
pts <- pts[!(pts$result == 1 & !(pts$Aivh5313 == "PÅVISAD" | pts$Aivh7313 == "PÅVISAD")),]
pts$Namn[pts$Namn == ""] <- pts$Ringmärkning[pts$Namn == ""]
##View(pts@data[order(pts@data$Ankomstdatum),])
pts <- data.frame(species = pts$Djurslag,
                  status = pts$result,
                  ViltID = pts$Namn,
                  date = pts$Ankomstdatum,
                  stringsAsFactors = FALSE)
## Drop if the sample is not yet complete:
pts <- pts[pts$status != 2,]
df <- time_count(pts$date[pts$status == 1],
           "months",
           "freq",
           tmin = min(pts$date, na.rm = TRUE),
           tmax = max(pts$date, na.rm = TRUE))
names(df)[names(df) == "n"] <- "pos"
df$neg <- time_count(pts$date[pts$status == 0],
           "months",
           "freq",
           tmin = min(pts$date, na.rm = TRUE),
           tmax = max(pts$date, na.rm = TRUE))$n
df$months <- paste0(months(as.POSIXlt(as.Date(df$months))),"-" ,as.POSIXlt(as.Date(df$months))$year+1900)
## Write to web
data <- tempfile()
graph <- tempfile()
my_y_axis <- yAxes(list(yAxis("a", "linear", "left", NULL, NULL, display = TRUE, labelString = "Antal fåglar per månad")))
writeLines(timeseries_html("data", "ai_data.js", yAxes = my_y_axis), graph)
writeLines(timeseries_json(df = df,
                           dataname = "data",
                           x = "months",
                           series_label = c("Positiva",
                                            "Negativa"),
                           type = c("bar", "bar"),
                           backgroundColor = c("#D22630", "#00A9CE"),
                           hoverBackgroundColor = c("#B90D17", "#00769B"),
                           fill = c(FALSE),
                           hidden = c(FALSE, FALSE),
                           pointRadius = 0,
                           yAxisID = c("a", "a")), data)
file.copy(graph, "/media/ESS_webpages/AI/ai_timeseries.html", overwrite = TRUE)
file.copy(data, "/media/ESS_webpages/AI/ai_data.js", overwrite = TRUE)
