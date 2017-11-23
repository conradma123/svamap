##' time_count
##'
##' Count the number of values in a date vector binned by various
##' intervals.
##'
##' @title time_count
##' @param time a vector of dates
##' @param breaks one of c('days', 'weeks', 'months', 'years') to
##'     split the data by
##' @param count 'freq' for frequency count or 'cumul' for cumulative
##'     count
##' @param tmin starting date (optional)
##' @param tmax ending date (optional)
##' @export
##' @return df dataframe containing the frequency or cumulative count of selected time breaks
##' @author Thomas Rosendal
time_count<-function(time,
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
##' timeseries_json
##'
##' Convert a dataframe with temporal counts into a json object that
##' can be written to a file that will be read by a timeseries graph
##' in chart.js.
##'
##' @title timeseries_json
##' @param df A dataframe with one column that has a date and the
##'     remaining columns will be plotted in the final graph
##' @param x The name of the column in the df that has dates. This
##'     will be the x-axis of the graph
##' @param dataname The name of the data in the .js. This object name
##'     will be called in the html.
##' @param backgroundColor The background hex colour of the bars or
##'     line.
##' @param hoverBackgroundColor The hex colour that the series changes
##'     to when you hover over with the mouse.
##' @param borderColor The hex border colour of the series
##' @param type The type of series, either "line" or "bar"
##' @param pointRadius The radius of the points in a line series
##' @param lineTension The besier tension of the line series. Set to 0
##'     if you want no smoothing effect.
##' @param fill The fill below the line. Set to TRUE if you want to
##'     fill the area under the curve.
##' @param hidden When the plot is first loaded, should the series be
##'     hidden or visible. You can afterwards click on the graph
##'     series to unhide it or hide it. So this is just the default
##'     state and it TRUE or FALSE
##' @param series_label The string name of the series for the legend
##' @param yAxisID The ID of the yaxis. Used if you have multiple y axes.
##' @return A character vector
##' @export
##' @author Thomas Rosendal
timeseries_json <- function(df,
                            x,
                            dataname = "data",
                            backgroundColor = c("#D22630", "#00A9CE", "#43B02A", "#F2A900"),
                            hoverBackgroundColor = c("#D22630", "#00A9CE", "#43B02A", "#F2A900"),
                            borderColor = NULL,
                            type = c("bar"),
                            pointRadius = NA,
                            lineTension = NA,
                            fill = FALSE,
                            hidden = FALSE,
                            series_label = NULL,
                            yAxisID = NULL) {
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
    ## Y Axis assignment
    if(is.null(yAxisID)){
        yAxisID <- rep(NA, length(names(df)[names(df) != x]))
    } else {
        stopifnot(length(yAxisID) == length(names(df)[names(df) != x]))
    }
    names(yAxisID) <- names(df)[names(df) != x]
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
        if(is.na(yAxisID[y])) {
            yAxisID_i <- NULL
        } else {
            yAxisID_i <- paste0("yAxisID: '", yAxisID[y], "'")
        }
        hidden <- paste0("hidden: ", hidden[y])
        type <- paste0("type: '", type[y], "'")
        paste("{", paste(c(label,
                           data,
                           yAxisID_i,
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

##' timeseries_html
##'
##' A method to generate the nessessary html for a timeseries plot
##' using chart.js.
##' @title timeseries_html
##' @param dataname The name of the data object to be loaded in the
##'     html. This should match your call in the json function.
##' @param datafilename The name of the .js file that you need to load
##'     from the html
##' @param yAxes The specification of the yaxis, of axes. This must be
##'     of classy(Axes) == "yAxes" and is a list of objects generated
##'     by the svamap::yAxis() function
##' @return A character vector
##' @export
##' @author Thomas Rosendal
timeseries_html <- function(dataname = "data",
                            datafilename = "data.js",
                            yAxes) {
    stopifnot(class(yAxes) == "yAxes")
    head <- c("    <title>Bar Chart</title>",
              "    <meta charset='utf-8' />",
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
              "\t\t  yAxes:", yAxes,
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
##' yAxes
##'
##' stick together you various yAxis objects into a yAxes object.
##' @title yAxes
##' @param x A list of yAxis objects
##' @return a character vector of class 'yAxes'
##' @export
##' @author Thomas Rosendal
yAxes <- function(x) {
    stopifnot(is.list(x))
    yAxes <- paste0("[", paste(x, collapse = ",\n"), "]")
    class(yAxes) <- "yAxes"
    return(yAxes)
}
##' yAxis
##'
##' Generate the necessary text for a yAxis object. These will then be
##' passed the the yAxes() function
##' @title yAxis
##' @param id The id of the yAxis. This should match the id you pass
##'     in your data if you have more than one yaxis.
##' @param type The only supported type now id 'linear'
##' @param position Where should be axis be 'right' of 'left'?
##' @param min The minimum value of the axis.
##' @param max The maximum value of the axis.
##' @param display Do you want to display a title on the axis? TRUE of FALSE
##' @param labelString The label of the axis
##' @return A character vector
##' @export
##' @author Thomas Rosendal
yAxis <- function(id,
                  type = c("linear"),
                  position = c('left', 'right'),
                  min,
                  max,
                  display = TRUE,
                  labelString) {
    stopifnot(length(id) == 1)
    stopifnot(is.character(id))
    stopifnot(length(type) == 1)
    stopifnot(is.character(type))
    stopifnot(length(position) == 1)
    match.arg(position)
    match.arg(type)
    id <- paste0("id:'", id, "'")
    type <- paste0("type:'", type, "'")
    position <- paste0("position:'", position, "'")
    if(!is.null(ticks(min, max))) {
        ticks_loc<- paste("ticks:", ticks(min, max))
    } else {
        ticks_loc<- NULL
    }
    scaleLabel <- paste("scaleLabel:", scaleLabel(display, labelString))
    yAxis <- paste0("{", paste(c(id,
                                 type,
                                 position,
                                 ticks_loc,
                                 scaleLabel), collapse = ",\n"),
                    "}")
    class(yAxis) <- "yAxis"
    return(yAxis)
}
##' ticks
##'
##' Generate the tick object.
##' @title ticks
##' @param min the minimum value
##' @param max the maximum value
##' @return A character vector
##' @author Thomas Rosendal
ticks <- function(min, max) {
    if(is.null(min) & is.null(max)) return(NULL)
    stopifnot(max > min)
    ticks <- paste0("{max:", max, ",min:", min, "}")
}
##' scaleLabel
##'
##' generate the scaleLabel object
##' @title scaleLabel
##' @param display Should the label be visible
##' @param labelString The label
##' @return A character vector
##' @author Thomas Rosendal
scaleLabel <- function(display, labelString) {
    stopifnot(is.logical(display))
    stopifnot(length(display) == 1)
    stopifnot(is.character(labelString))
    stopifnot(length(labelString) == 1)
    if(display) {
        display <- 'true'
    } else {
        display <- 'false'
    }
    paste0("{display:", display, ",labelString: '", labelString, "'}")
}
##' ma
##'
##' The moving average of a set of values. This function calculates
##' the moving average across an interval. The interval is no
##' consistently large as the extreme left and right of the vector
##' since the interval 'walks' off the end of the vector where there
##' are no values. The effect is that if you have a left interval of 2
##' and right interval of 2. The returns 1st index in the vector is
##' the mean(x[1:3]), the 2nd is mean(x[1:4]), the 3rd is
##' mean(x[1:5]), the 4th is mean(x[2:6]), the 5th is mean(x[3:7]) and
##' so on. At the right end of the vector the same occurs in the
##' opposite direction. The alternative approach is to return NA for
##' return indexes 1:2 because you cannot calculate the moving average
##' with a left interval of 2 for those values; this mthod is not
##' implemented.
##' @title ma
##' @param x A numeric vector
##' @param left_interval an integer value of the number of values to
##'     include in the average to the LEFT of a given position
##' @param right_interval an integer value of the number of values to
##'     include in the average to the RIGHT of a given position
##' @param na.rm Should be na values be ignored?
##' @return A numeric vector
##' @export
##' @author Thomas Rosendal
ma <- function(x,
               left_interval = 1,
               right_interval = 1,
               na.rm = TRUE) {
    stopifnot(is.numeric(x))
    stopifnot(left_interval%%1==0)
    stopifnot(right_interval%%1==0)
    do.call("c", lapply(seq_len(length(x)), FUN = function(y){
        a <- ifelse(y-left_interval < 0, 0, y-left_interval)
        b <- ifelse(y+right_interval > length(x), length(x), y+right_interval)
        mean(x[a:b], na.rm = na.rm)
    }))
}
