library(svamap)
##
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
########################
## Drop data that you don't want to display from points
########################
pts@data <- data.frame(species = pts@data$Djurslag,
                       result = pts@data$Status..numerisk.,
                       date = as.Date(pts@data$Ankomstdatum),
                       stringsAsFactors = FALSE)
##Count over time
df <- time_count(pts@data$date,
           "months",
           "freq",
           tmin = min(pts@data$date),
           tmax = max(pts@data$date))
names(df)[names(df) == "n"] <- "total"
for(i in unique(pts@data$species)) {
    df[,i] <- time_count(pts@data$date[pts@data$species == i],
                         "months",
                         "freq",
                         tmin = min(pts@data$date),
                         tmax = max(pts@data$date))$n
}
df
df$months <- paste0(months(as.POSIXlt(as.Date(df$months))),"-" ,as.POSIXlt(as.Date(df$months))$year+1900)
## Write to web
data <- tempfile()
graph <- tempfile()
my_y_axis <- yAxes(list(yAxis("a", "linear", "left", min = NULL, max = NULL, display = TRUE, labelString = "Number of animals sampled")))
writeLines(timeseries_html("data", "CWD_data.js", yAxes = my_y_axis), graph)
writeLines(timeseries_json(df = df,
                           dataname = "data",
                           x = "months",
                           series_label = c("All samples", unique(pts@data$species)),
                           type = rep("bar", length(unique(pts@data$species))+1),
                           backgroundColor = c("#696969", "#D22630", "#43B02A", "#00A9CE", "#F2A900"),
                           hoverBackgroundColor = c("#696969", "#D22630", "#43B02A", "#00A9CE", "#F2A900"),
                           fill = c(FALSE),
                           hidden = c(TRUE, rep(FALSE, length(unique(pts@data$species)))),
                           pointRadius = 0), data)
file.copy(graph, "CWD_timeseries.html", overwrite = TRUE)
file.copy(data, "CWD_data.js", overwrite = TRUE)
browseURL("CWD_timeseries.html")
