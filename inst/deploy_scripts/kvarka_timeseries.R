##
##' @title time.count
##'
##'
##' @param time a vector of dates
##' @param breaks one of c('days', 'weeks', 'months', 'years') to split the data by
##' @param count 'freq' for frequency count or 'cumul' for cumulative count
##' @param tmin starting date (optional)
##' @param tmax ending date (optional)
##'
##' @return df dataframe containing the frequency or cumulative count of selected time breaks
time.count<-function(time,
                     breaks=c('days', 'weeks', 'months', 'years'),
                     count=c('freq', 'cumul'),
                     tmin=NULL,
                     tmax=NULL) {
    if (!is.null(tmin)) {
        time <- time[time >= tmin]
    } else {
        tmin <- min(time, na.rm = TRUE)
    }
    if (!is.null(tmax)) {
        time <- time[time <  tmax]
    } else {
        tmax <- max(time, na.rm = TRUE)
    }
    time_running <- seq(tmin, tmax - 1, 1)
    time <- c(time, time_running)   #make sure all dates are included
    breaks = match.arg(breaks)
    count = match.arg(count)
    ## Summarize by break
    df <- as.Date(as.character(cut(time, breaks=breaks)))
    df <- as.data.frame(table(df))
    names(df) <- c(breaks, "n")
    ## Summarize the running time by break
    df_running <- as.Date(as.character(cut(time_running, breaks=breaks)))
    df_running <- as.data.frame(table(df_running))
    names(df_running) <- c(breaks, "n")
    df$n <- df$n - df_running$n    #remove the added dates
    if (count=='cumul') { df$n <- cumsum(df$n) }
    return(df)
}
## Convert df to json
timeseries_json <- function(df,
                            x,
                            dataname = "data",
                            col = c("#D22630", "#00A9CE", "#43B02A", "#F2A900"),
                            hidden = FALSE,
                            series_label = NULL) {
    stopifnot(x %in% names(df))
    stopifnot(length(col) != 0)
    if(length(names(df)[names(df) != x]) != length(col)) {
        col <- rep(col, length.out = length(names(df)[names(df) != x]))
    }
    names(col) <- names(df)[names(df) != x]
    if(is.null(series_label)){
        series_label <- names(df)[names(df) != x]
    }
    if(length(series_label) != length(names(df)[names(df) != x])){
        series_label <- names(df)[names(df) != x]
        warning("length of series_label not equal to the number of series, reverting to column names of dataframe")
    }
    if(length(hidden) == 1) {
        hidden <- rep(hidden, length(names(df)[names(df) != x]))
    }
    if(length(hidden) != length(names(df)[names(df) != x])){
        hidden <- rep(FALSE, length(names(df)[names(df) != x]))
        warning("length of hidden not equal to the number of series, unhiding all series")
    }
    hiddennew <- hidden
    hiddennew[hidden] <- "true"
    hiddennew[!hidden] <- "false"
    hidden <- hiddennew
    names(hidden) <- names(df)[names(df) != x]
    names(series_label) <- names(df)[names(df) != x]
    labels <- paste0("['", paste(as.character(df[,x]), collapse = "', '"), "']")
    ##
    datasets <- paste0("[", paste(lapply(names(df)[names(df) != x], function(y){
        label <- paste0("label: '", series_label[y], "',")
        data <- paste0("data: [", paste(df[,y], collapse = ", "), "],")
        backgroundColor <- paste0("backgroundColor:",  " '", col[y], "',")
        hidden <- paste0("hidden: ", hidden[y])
        paste("{",label, data, backgroundColor, hidden, "}", sep = "\n")
    }), collapse = ",\n"), "]")
    ##
    paste0(dataname, " = {labels: ", labels, ",\ndatasets: ", datasets, "\n}")
}
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
df$frac <- df$pos/(df$pos+df$neg)
## Write to web
writeLines(timeseries_json(df = df,
                           x = "months",
                           series_label = c("Pos",
                                            "Neg",
                                            "frac"),
                           hidden = c(FALSE, FALSE, FALSE)), "data1.js")

## If you want to set attributes of a line then use the follwoing in the data:
## fill: false,
## borderColor: '#43B02A',
## backgroundColor:'#43B02A',
## hidden: false,
## type: 'line',
## pointRadius: 1,
## lineTension: 0.2
