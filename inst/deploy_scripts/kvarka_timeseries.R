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

df <- time.count(kvarka_data_map$date[kvarka_data_map$status == 1],
           "months",
           "freq",
           tmin = min(kvarka_data_map$date),
           tmax = max(kvarka_data_map$date))
names(df)[names(df) == "n"] <- "pos"
df$neg <- time.count(kvarka_data_map$date[kvarka_data_map$status == 0],
           "months",
           "freq",
           tmin = min(kvarka_data_map$date),
           tmax = max(kvarka_data_map$date))$n
df$frac <- 100*df$pos/(df$pos+df$neg)
ma <- function(x, left_interval = 1, right_interval = 1, na.rm = TRUE) {
    do.call("c", lapply(seq_len(length(x)), FUN = function(y){
        a <- ifelse(y-left_interval < 0, 0, y-left_interval)
        b <- ifelse(y+right_interval > length(x), length(x), y+right_interval)
        mean(x[a:b], na.rm = na.rm)
    }))
}
df$frac <- ma(df$frac, 1, 1)
df <- df[,c(1, 4, 2, 3)]
df$months <- paste0(months(as.POSIXlt(as.Date(df$months))),"-" ,as.POSIXlt(as.Date(df$months))$year+1900)
## Write to web
writeLines(timeseries_html("data3", "data3.js"), "foo.html")
writeLines(timeseries_json(df = df,
                           dataname = "data3",
                           x = "months",
                           series_label = c("Percent (3-month moving average)",
                                            "Positive",
                                            "Negative"),
                           type = c("line", "bar", "bar"),
                           fill = c(FALSE),
                           hidden = c(FALSE, FALSE, FALSE)), "data3.js")
