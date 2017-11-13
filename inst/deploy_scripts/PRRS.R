library(svamap)
library(ggplot2)
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
    if (!is.null(tmin)) {time <- time[time >= tmin]}
    if (!is.null(tmax)) {time <- time[time <  tmax]}
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
        col <- rep(col[1], length(names(df)[names(df) != x]))
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

df <- read.csv2("/media/t/Falkenrapporter/PRRS-2017-falkenrapport.csv", stringsAsFactors = FALSE)
df$Ankomstdatum <- as.Date(df$Ankomstdatum)
t_breaks <- as.Date(c("2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01"))
## and summarize by month
monthly <- time.count(df$Ankomstdatum, "months", "freq", tmin = t_breaks[1], tmax = t_breaks[5])
monthly$months <- as.Date(monthly$months)
names(monthly)[names(monthly) == "n"] <- "count_sample"
monthly$cumul_sample <- do.call("rbind", lapply(1:4, function(x){
    time.count(df$Ankomstdatum, "months", "cumul", tmin = t_breaks[x], tmax = t_breaks[x + 1])
}))$n
monthly$count_PPN <- do.call("rbind", lapply(1:4, function(x){
    temp <- df[df$Ankomstdatum >= t_breaks[x] & df$Ankomstdatum < t_breaks[x + 1],]
    temp <- temp[!duplicated(temp$PPN), ]
    time.count(temp$Ankomstdatum, "months", "freq", tmin = t_breaks[x], tmax = t_breaks[x + 1])
}))$n
monthly$cumul_PPN <- do.call("rbind", lapply(1:4, function(x){
    temp <- df[df$Ankomstdatum >= t_breaks[x] & df$Ankomstdatum < t_breaks[x + 1],]
    temp <- temp[!duplicated(temp$PPN), ]
    time.count(temp$Ankomstdatum, "months", "cumul", tmin = t_breaks[x], tmax = t_breaks[x + 1])
}))$n
## Write to web
writeLines(timeseries_json(df = monthly,
                           x = "months",
                           series_label = c("Number of samples per month",
                                            "Cumulative number of samples",
                                            "Number of new PPN per month",
                                            "Cummulative number of PPN"),
                           hidden = c(FALSE, TRUE, FALSE, TRUE)), "data1.js")
file.copy("data1.js", "/media/ESS_webpages/PRRS/", overwrite = TRUE)
file.copy("graph.html", "/media/ESS_webpages/PRRS/", overwrite = TRUE)
