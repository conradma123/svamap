library(svamap)
### Clean data
df <- read.csv2("/media/t/Falkenrapporter/PRRS-2017-falkenrapport.csv", stringsAsFactors = FALSE)
df$Ankomstdatum <- as.Date(df$Ankomstdatum)
t_breaks <- as.Date(c("2017-01-01", "2018-01-01"))
# Summarize the latest year by month
fix_data <- function(df, t_breaks) {
    monthly <- time_count(df$Ankomstdatum, "months", "freq", tmin = t_breaks[1], tmax = t_breaks[2])
    monthly$months <- as.Date(monthly$months)
    names(monthly)[names(monthly) == "n"] <- "count_sample"
    monthly$cumul <- time_count(df$Ankomstdatum, "months", "cumul", tmin = t_breaks[1], tmax = t_breaks[2])$n
    monthly$months <- months(monthly$months)
    return(monthly)
}
## Just 2017
df <- df[df$Ankomstdatum >=t_breaks[1] & df$Ankomstdatum < t_breaks[2],]
df_abbatoir <- df[df$Överordnadeuppdrag == "Ö09-022",]
df_sows <- df[df$Överordnadeuppdrag == "Ö09-021",]
vets <- unique(df_sows$Insändare)
abbatoirs <- unique(df_abbatoir$Insändare)
## The finishers:
data <- lapply(seq_len(length(abbatoirs)), function(y){
    timeseries_json(df = fix_data(df_abbatoir[df_abbatoir$Insändare == abbatoirs[y],], t_breaks),
                    x = "months",
                    dataname = paste0("data", y),
                    series_label = c("Number of samples per month",
                                     "Cumulative number of samples"),
                    backgroundColor = c("#860000", "#005D82"),
                    hoverBackgroundColor = c("#6D0000", "#004469"),
                    hidden = c(FALSE, TRUE),
                    fill = FALSE,
                    pointRadius = 2,
                    type = c("bar", "bar"))
    })
data <- paste(data, collapse = "\n")
my_y_axis <- yAxes(list(yAxis("a", "linear", "left", NULL, NULL, display = TRUE, labelString = "Number of samples")))
temp <- timeseries_html("data1", "data1.js", my_y_axis)
bodies <- lapply(seq_len(length(abbatoirs)), function(y){
    c("<div style = 'float: left;'><h3>", abbatoirs[y],"</h3>",
      gsub("data1", paste0("data", y), gsub("myChart", paste0("myChart", y), temp[(grep("<body>", temp)+1) : (grep("</body>", temp)-1)])),
      "</div>")
})
page <- do.call("c", c(temp[1:grep("<body>", temp)],
  bodies,
  temp[grep("</body>", temp):length(temp)]
  ))
writeLines(data, "/media/ESS_webpages/PRRS_timeseries_fin/data1.js")
writeLines(page, "/media/ESS_webpages/PRRS_timeseries_fin/graph.html")
## The sows:
data <- lapply(seq_len(length(vets)), function(y){
    timeseries_json(df = fix_data(df_sows[df_sows$Insändare == vets[y],], t_breaks),
                    x = "months",
                    dataname = paste0("data", y),
                    series_label = c("Number of samples per month",
                                     "Cumulative number of samples"),
                    backgroundColor = c("#860000", "#005D82"),
                    hoverBackgroundColor = c("#6D0000", "#004469"),
                    hidden = c(FALSE, TRUE),
                    fill = FALSE,
                    pointRadius = 2,
                    type = c("bar", "bar"))
    })
data <- paste(data, collapse = "\n")
my_y_axis <- yAxes(list(yAxis("a", "linear", "left", NULL, NULL, display = TRUE, labelString = "Number of samples")))
temp <- timeseries_html("data1", "data1.js", my_y_axis)
bodies <- lapply(seq_len(length(vets)), function(y){
    c("<div style = 'float: left;'><h3>", vets[y],"</h3>",
      gsub("data1", paste0("data", y), gsub("myChart", paste0("myChart", y), temp[(grep("<body>", temp)+1) : (grep("</body>", temp)-1)])),
      "</div>")
})
page <- do.call("c", c(temp[1:grep("<body>", temp)],
  bodies,
  temp[grep("</body>", temp):length(temp)]
  ))
writeLines(data, "/media/ESS_webpages/PRRS_timeseries_sow/data1.js")
writeLines(page, "/media/ESS_webpages/PRRS_timeseries_sow/graph.html")
