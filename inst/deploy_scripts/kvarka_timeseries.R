library(svamap)
##
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
df <- time_count(kvarka_data_map$date[kvarka_data_map$status == 1],
           "months",
           "freq",
           tmin = min(kvarka_data_map$date),
           tmax = max(kvarka_data_map$date))
names(df)[names(df) == "n"] <- "pos"
df$neg <- time_count(kvarka_data_map$date[kvarka_data_map$status == 0],
           "months",
           "freq",
           tmin = min(kvarka_data_map$date),
           tmax = max(kvarka_data_map$date))$n
df$frac <- 100*df$pos/(df$pos+df$neg)
df$frac <- round(svamap::ma(df$frac, 1, 1), 1)
df <- df[,c(1, 4, 2, 3)]
df$months <- paste0(months(as.POSIXlt(as.Date(df$months))),"-" ,as.POSIXlt(as.Date(df$months))$year+1900)
## Write to web
data <- tempfile()
graph <- tempfile()
my_y_axis <- yAxes(list(yAxis("a", "linear", "right", 0, 100, display = TRUE, labelString = "3-month moving average of percent positive"),
                        yAxis("b", "linear", "left", NULL, NULL, display = TRUE, labelString = "Number of samples per month")))
writeLines(timeseries_html("data", "kvarka_data.js", yAxes = my_y_axis), graph)
writeLines(timeseries_json(df = df,
                           dataname = "data",
                           x = "months",
                           series_label = c("Percent (3-month moving average)",
                                            "Positive",
                                            "Negative"),
                           type = c("line", "bar", "bar"),
                           backgroundColor = c("#696969", "#D22630", "#00A9CE"),
                           hoverBackgroundColor = c("#505050", "#B90D17", "#00769B"),
                           fill = c(FALSE),
                           hidden = c(FALSE, FALSE, FALSE),
                           pointRadius = 0,
                           yAxisID = c("a", "b", "b")), data)
file.copy(graph, "/media/ESS_webpages/kvarka/kvarka_timeseries.html", overwrite = TRUE)
file.copy(data, "/media/ESS_webpages/kvarka/kvarka_data.js", overwrite = TRUE)
