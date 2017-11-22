library(svamap)
### Clean data
df <- read.csv2("/media/t/Falkenrapporter/PRRS-2017-falkenrapport.csv", stringsAsFactors = FALSE)
df$Ankomstdatum <- as.Date(df$Ankomstdatum)
t_breaks <- as.Date(c("2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01"))
# Summarize the latest year by month
fix_data <- function(df, t_breaks) {
    monthly <- time_count(df$Ankomstdatum, "months", "freq", tmin = t_breaks[4], tmax = t_breaks[5])
    monthly$months <- as.Date(monthly$months)
    names(monthly)[names(monthly) == "n"] <- "count_sample"
    monthly$cumul <- time_count(df$Ankomstdatum, "months", "cumul", tmin = t_breaks[4], tmax = t_breaks[5])$n
    ## Take data from first 3 years and generate an 'expected':
    monthly$hist_count <- round(rowMeans(do.call("cbind", lapply(1:3, function(x){
        time_count(df$Ankomstdatum, "months", "freq", tmin = t_breaks[x], tmax = t_breaks[x+1])$n
    }))))
    monthly$hist_cumul <- round(rowMeans(do.call("cbind", lapply(1:3, function(x){
        time_count(df$Ankomstdatum, "months", "cumul", tmin = t_breaks[x], tmax = t_breaks[x+1])$n
    }))))
    monthly$months <- months(monthly$months)
    monthly <- monthly[,c(1,4,5,2,3)]
    return(monthly)
}
df_abbatoir <- df[df$Överordnadeuppdrag == "Ö09-022",]
df_sows <- df[df$Överordnadeuppdrag == "Ö09-021",]
## Write to web
data <- tempfile()
graph <- tempfile()
## The finishers:
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
                           type = c("line", "line", "bar", "bar")), data)
my_y_axis <- yAxes(list(yAxis("a", "linear", "left", NULL, NULL, display = TRUE, labelString = "Number of finisher samples collected")))
writeLines(timeseries_html("data1", "data1.js", my_y_axis), graph)
file.copy(data, "/media/ESS_webpages/PRRS/data1.js", overwrite = TRUE)
file.copy(graph, "/media/ESS_webpages/PRRS/graph_abbatoir.html", overwrite = TRUE)
## The sows
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
                           yAxisID = c("a", "a", "a", "a"),
                           type = c("line", "line", "bar", "bar")), data)
my_y_axis <- yAxes(list(yAxis(id = "a", type = "linear", position = "left", max = NULL, min = NULL, display = TRUE, labelString = "Number of sow samples collected")))
writeLines(timeseries_html("data2", "data2.js", my_y_axis), graph)
file.copy(data, "/media/ESS_webpages/PRRS/data2.js", overwrite = TRUE)
file.copy(graph, "/media/ESS_webpages/PRRS/graph_sows.html", overwrite = TRUE)
