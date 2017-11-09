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
    if (!is.null(tmin)) {time <- time[time>=tmin]}
    if (!is.null(tmax)) {time <- time[time<=tmax]}
    time_running <- seq(tmin, tmax, 1)
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

df <- read.csv2("/media/t/Falkenrapporter/PRRS-2017-falkenrapport.csv", stringsAsFactors = FALSE)
df$Ankomstdatum <- as.Date(df$Ankomstdatum)
weekly <- time.count(df$Ankomstdatum, "weeks", "cumul", tmin = as.Date("2017-01-01"), tmax = as.Date("2017-12-31"))
weekly$weeks <- as.Date(weekly$weeks)
weekly$cumul_sample <- weekly$n
weekly$sample <- time.count(df$Ankomstdatum, "weeks", tmin = as.Date("2017-01-01"), tmax = as.Date("2017-12-31"))$n
df2 <- df[!duplicated(df$PPN),]
weekly$PPN <- time.count(df2$Ankomstdatum, "weeks", tmin = as.Date("2017-01-01"), tmax = as.Date("2017-12-31"))$n
weekly$cumul_PPN <- time.count(df2$Ankomstdatum, "weeks", "cumul", tmin = as.Date("2017-01-01"), tmax = as.Date("2017-12-31"))$n
weekly
## Number of samples per week
gg <- ggplot(weekly, aes(x = weeks, y = sample)) +
    geom_bar(stat = "identity", fill = "#D22630", colour = "grey40", width = 5) +
    scale_x_date(limits = as.Date(c("2016-12-01", "2018-01-01")), date_breaks = "month")
gg
## Cumulative number of samples per week
gg <- ggplot(weekly, aes(x = weeks, y = cumul_sample)) +
    geom_bar(stat = "identity", fill = "#D22630", colour = "grey40", width = 5) +
    scale_x_date(limits = as.Date(c("2016-12-01", "2018-01-01")), date_breaks = "month")
gg
## number of new PPN per week
gg <- ggplot(weekly, aes(x = weeks, y = PPN)) +
    geom_bar(stat = "identity", fill = "#D22630", colour = "grey40", width = 5) +
    scale_x_date(limits = as.Date(c("2016-12-01", "2018-01-01")), date_breaks = "month")
gg
## cumulative number of PPN per week
gg <- ggplot(weekly, aes(x = weeks, y = cumul_PPN)) +
    geom_bar(stat = "identity", fill = "#D22630", colour = "grey40", width = 5) +
    scale_x_date(limits = as.Date(c("2016-12-01", "2018-01-01")), date_breaks = "month")
gg


## drop unneccessary columns
weekly <- weekly[,names(weekly) != "n"]

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
    hiddennew[hidden] <- "'true'"
    hiddennew[!hidden] <- "'false'"
    hidden <- hiddennew
    names(series_label) <- names(df)[names(df) != x]
    labels <- paste0("['", paste(as.character(df[,x]), collapse = "', '"), "']")
    datasets <- paste0("[", paste(lapply(names(df)[names(df) != x], function(y){
        label <- paste0("label: '", series_label[y], "',")
        data <- paste0("data: [", paste(df[,y], collapse = ", "), "],")
        backgroundColor <- paste0("backgroundColor:",  " '", col[y], "',")
        hidden <- paste0("hidden: '", hidden[y], "'")
        paste("{",label, data, backgroundColor, hidden, "}", sep = "\n")
    }), collapse = ",\n"), "]")
    paste0(dataname, " = {labels: ", labels, ",\ndatasets: ", datasets, "\n}")
}

writeLines(timeseries_json(df = weekly,
                           x = "weeks",
                           series_label = c("Cumulative number of Samples",
                                            "Number of samples per week",
                                            "Number of new PPN per week",
                                            "Cummulative number of PPN"),
                           hidden = c(TRUE, FALSE, FALSE, TRUE)), "data1.js")
file.copy("data1.js", "/media/ESS_webpages/PRRS/")
file.copy("graph.html", "/media/ESS_webpages/PRRS/")
