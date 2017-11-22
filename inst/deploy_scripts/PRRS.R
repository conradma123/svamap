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
                            backgroundColor = c("#D22630", "#00A9CE", "#43B02A", "#F2A900"),
                            hoverBackgroundColor = c("#D2263080", "#00A9CE80", "#43B02A80", "#F2A90080"),
                            borderColor = NULL,
                            type = c("bar"),
                            pointRadius = NA,
                            lineTension = NA,
                            fill = NA,
                            hidden = FALSE,
                            series_label = NULL) {
    ## Check if the data is in the dataframe
    stopifnot(x %in% names(df))
    ## Chart types
    stopifnot(length(type) != 0)
    type <- rep(type, length.out = length(names(df)[names(df) != x]))
    names(type) <- names(df)[names(df) != x]
    ## Colours
    stopifnot(length(col) != 0)
    backgroundColor <- rep(backgroundColor, length.out = length(names(df)[names(df) != x]))
    names(backgroundColor) <- names(df)[names(df) != x]
    ## border colour
    if(is.null(borderColor)){
        borderColor <- backgroundColor
    } else {
        borderColor <- rep(borderColor, length.out = length(names(df)[names(df) != x]))
        names(borderColor) <- names(df)[names(df) != x]
    }
    ## hoverBackgroundColor
    if(is.null(hoverBackgroundColor)){
        hoverBackgroundColor <- backgroundColor
    } else {
        hoverBackgroundColor <- rep(hoverBackgroundColor, length.out = length(names(df)[names(df) != x]))
        names(hoverBackgroundColor) <- names(df)[names(df) != x]
    }
    ## Labels
    if(is.null(series_label)){
        series_label <- names(df)[names(df) != x]
    }
    if(length(series_label) != length(names(df)[names(df) != x])){
        series_label <- names(df)[names(df) != x]
        warning("length of series_label not equal to the number of series, reverting to column names of dataframe")
    }
    names(series_label) <- names(df)[names(df) != x]
    ## Hidden or unhidden series
    stopifnot(is.logical(hidden))
    hidden <- rep(hidden, length.out = length(names(df)[names(df) != x]))
    hiddennew <- hidden
    hiddennew[hidden] <- "true"
    hiddennew[!hidden] <- "false"
    hidden <- hiddennew
    names(hidden) <- names(df)[names(df) != x]
    ## Point radius and line tension and fill for line graphs
    stopifnot(length(pointRadius) != 0)
    pointRadius <- rep(pointRadius, length.out = length(names(df)[names(df) != x]))
    names(pointRadius) <- names(df)[names(df) != x]
    stopifnot(length(lineTension) != 0)
    lineTension <- rep(lineTension, length.out = length(names(df)[names(df) != x]))
    names(lineTension) <- names(df)[names(df) != x]
    stopifnot(is.logical(fill))
    stopifnot(length(fill) != 0)
    fill <- rep(fill, length.out = length(names(df)[names(df) != x]))
    fillnew <- fill
    fillnew[fill] <- "true"
    fillnew[!fill] <- "false"
    fill <- fillnew
    names(fill) <- names(df)[names(df) != x]
    ## X axis labels
    labels <- paste0("['", paste(as.character(df[,x]), collapse = "', '"), "']")
    ##
    datasets <- paste0("[\n    ", paste(lapply(names(df)[names(df) != x], function(y){
        label <- paste0("label: '", series_label[y], "'")
        data <- paste0("data: [", paste(df[,y], collapse = ", "), "]")
        backgroundColor_i <- paste0("backgroundColor: '", backgroundColor[y], "'")
        borderColor_i <- paste0("borderColor: '", borderColor[y], "'")
        hoverBackgroundColor_i <- paste0("hoverBackgroundColor: '", hoverBackgroundColor[y], "'")
        if(is.na(pointRadius[y])) {
            pointRadius_i <- NULL
        } else {
            pointRadius_i <- paste0("pointRadius: ", pointRadius[y])
        }
        if(is.na(lineTension[y])) {
            lineTension_i <- NULL
        } else {
            lineTension_i <- paste0("lineTension: ", lineTension[y])
        }
        if(is.na(fill[y])) {
            fill_i <- NULL
        } else {
            fill_i <- paste0("fill: ", fill[y])
        }
        hidden <- paste0("hidden: ", hidden[y])
        type <- paste0("type: '", type[y], "'")
        paste("{", paste(c(label,
                           data,
                           backgroundColor_i,
                           borderColor_i,
                           hoverBackgroundColor_i,
                           pointRadius_i,
                           lineTension_i,
                           fill_i,
                           hidden,
                           type), collapse = ",\n"), "}")
    }), collapse = ",\n    "), "]")
    ##
    paste0(dataname, " = {\n    labels: ", labels, ",\n    datasets: ", datasets, "\n}")
}

timeseries_html <- function(dataname = "data", datafilename = "data.js") {
    head <- c("    <title>Bar Chart</title>",
              "    <script src=\"http://www.chartjs.org/dist/2.7.1/Chart.bundle.js\"></script>",
              paste0("    <script src='", datafilename, "'></script>"))

    body <- c("    <canvas id=\"myChart\" width=\"400\"></canvas>",
              "    <script>",
              "      var ctx = document.getElementById(\"myChart\").getContext('2d');",
              "      var myChart = new Chart(ctx, {",
              "\t  type: 'bar',",
              paste0("\t  data: ", dataname, ","),
              "\t  options: {",
              "\t      scales: {",
              "\t\t  yAxes: [{",
              "\t\t      ticks: {",
              "\t\t\t  beginAtZero:true",
              "\t\t      }",
              "\t\t  }]",
              "\t      }",
              "\t  }",
              "      });",
              "    </script>")
    html <- c("<!doctype html>",
              "<html>",
              "  <head>",
              head,
              "  </head>",
              "  <body>",
              body,
              "  </body>",
              "</html>")
    return(html)
}

df <- read.csv2("/media/t/Falkenrapporter/PRRS-2017-falkenrapport.csv", stringsAsFactors = FALSE)
df$Ankomstdatum <- as.Date(df$Ankomstdatum)
t_breaks <- as.Date(c("2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01"))
# Summarize the latest year by month

fix_data <- function(df, t_breaks) {
    monthly <- time.count(df$Ankomstdatum, "months", "freq", tmin = t_breaks[4], tmax = t_breaks[5])
    monthly$months <- as.Date(monthly$months)
    names(monthly)[names(monthly) == "n"] <- "count_sample"
    monthly$cumul <- time.count(df$Ankomstdatum, "months", "cumul", tmin = t_breaks[4], tmax = t_breaks[5])$n
    ## Take data from first 3 years and generate an 'expected':
    monthly$hist_count <- round(rowMeans(do.call("cbind", lapply(1:3, function(x){
        time.count(df$Ankomstdatum, "months", "freq", tmin = t_breaks[x], tmax = t_breaks[x+1])$n
    }))))
    monthly$hist_cumul <- round(rowMeans(do.call("cbind", lapply(1:3, function(x){
        time.count(df$Ankomstdatum, "months", "cumul", tmin = t_breaks[x], tmax = t_breaks[x+1])$n
    }))))
    monthly$months <- months(monthly$months)
    monthly <- monthly[,c(1,4,5,2,3)]
    return(monthly)
}
df_abbatoir <- df[df$Överordnadeuppdrag == "Ö09-022",]
df_sows <- df[df$Överordnadeuppdrag == "Ö09-021",]

## Write to web
writeLines(timeseries_json(df = fix_data(df_abbatoir, t_breaks),
                           x = "months",
                           dataname = "data1",
                           series_label = c("Expected Number of samples per month",
                                            "Expected Cumulative samples per month",
                                            "Number of samples per month",
                                            "Cumulative number of samples"),
                           backgroundColor = c("#860000", "#005D82", "#D22630", "#00A9CE"),
                           hoverBackgroundColor = c("#6D0000", "#004469", "#B90D17", "#0090B5"),
                           hidden = c(FALSE, TRUE, FALSE, TRUE),
                           fill = FALSE,
                           type = c("line", "line", "bar", "bar")), "data1.js")
writeLines(timeseries_html("data1", "data1.js"), "graph_abbatoir.html")

writeLines(timeseries_json(df = fix_data(df_sows, t_breaks),
                           x = "months",
                           dataname = "data2",
                           series_label = c("Expected Number of samples per month",
                                            "Expected Cumulative samples per month",
                                            "Number of samples per month",
                                            "Cumulative number of samples"),
                           backgroundColor = c("#860000", "#005D82", "#D22630", "#00A9CE"),
                           hoverBackgroundColor = c("#6D0000", "#004469", "#B90D17", "#0090B5"),
                           hidden = c(FALSE, TRUE, FALSE, TRUE),
                           fill = FALSE,
                           type = c("line", "line", "bar", "bar")), "data2.js")
writeLines(timeseries_html("data2", "data2.js"), "graph_sows.html")

file.copy("data1.js", "/media/ESS_webpages/PRRS/", overwrite = TRUE)
file.copy("graph_abbatoir.html", "/media/ESS_webpages/PRRS/", overwrite = TRUE)
file.copy("data2.js", "/media/ESS_webpages/PRRS/", overwrite = TRUE)
file.copy("graph_sows.html", "/media/ESS_webpages/PRRS/", overwrite = TRUE)


## To customize the axes one can add options to the html and then
## assign dataseries in the .js with yAxisID 'B' in the data for
## example, then that dataseries follows that axis. You can add an
## axis on the right and left if you wish and give them labels. The
## definition for two axes could be something like this in the html:
## options: {
##     scales: {
##         yAxes: [{
##             id: 'B',
##             type: 'linear',
##             position: 'right',
##             ticks: {
##       	  max: 100,
##       	  min:0
##             },
##             scaleLabel: {
##       	  display: true,
##       	  labelString: '3-Month moving average percent Positive'
##             }
##         }, {
##             id: 'A',
##             type: 'linear',
##             position: 'left',
##             ticks: {
##       	  beginAtZero:true
##             },
##             scaleLabel: {
##       	  display: true,
##       	  labelString: 'Number of Samples'
##             }
##         }]
##     }
## }
