##' Count and cumsum of data grouped by a specified time period
##' 
##' Reshape samples data extracted from Falken (Gröna Pucken) and cleaned through \code{read.gp()}
##' and gives back a data.frame object with count and cumsum of data grouped by a specified time period
##' 
##' @title samples_group
##' @author SVA
##' @return A data.frame object
##' @export
##' @param data_sample A data.frame object generated from the function \code{read.gp()}
##' @param breaks Method used to group the date field (week, month, day). Default to "week"
##' @param by Dataframe's field used to compute count and cumulative sum. Default to "status"
samples_group <- function (data_sample = read_falken(),
                     breaks = c("week", "month", "day"),
                     by = c("status", "Överordnat_uppdrag", "material"))
{
  
  breaks <- match.arg(breaks)
  by <- match.arg(by)
  
  data_sample$when <- as.POSIXct(cut(data_sample$Ankomstdatum, breaks = "week"))
  
  ## TODO: Fix groups of overordnat uppdrag separated by ','
  data_sample$label <- switch(by,
                       status = data_sample$status,
                       overordnat_uppdrag = data_sample$Överordnat_uppdrag,
                       material =  data_sample$Materialnamn)
  data_sample <- unique(data_sample[, c("when", "Provid", "label"), drop=FALSE])
  
  ## Create an index and tabulate
  data_sample$index <- paste0(data_sample$when, data_sample$label)
  
  count <- as.data.frame(table(data_sample$index),
                         stringsAsFactors=FALSE)
  names(count) <- c("index", "n")
  
  ## Expand to have data_sample covering the complete time period
  t_vec <- seq(from=min(data_sample$when), to=max(data_sample$when), by="day")
  t_vec <- unique(as.POSIXct(cut(t_vec, breaks = "week")))
  data_sample <- expand.grid(when = t_vec,
                      label = unique(data_sample$label),
                      KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  data_sample$index <- paste0(data_sample$when, data_sample$label)
  data_sample$n <- 0
  
  ## Match counts
  i <- match(data_sample$index, count$index)
  j <- which(!is.na(i))
  i <- i[!is.na(i)]
  data_sample$n[j] <- count$n[i]
  
  ## Calculate cumulative sum for each group of label
  data_sample$label[is.na(data_sample$label)] <- "NA"
  data_sample <- by(data_sample, data_sample$label, function(x) {
    x <- x[order(x$when),]
    x$Cumsum = cumsum(x$n)
    x
  })
  
  ## Collect data from by
  data_sample <- do.call("rbind", data_sample)
  
  ## Reset cumsum value from dates in future
  data_sample$Cumsum[data_sample$when > as.POSIXct(cut(Sys.Date(), breaks="week"))] <- 0
  
  ## Clean result
  data_sample <- data_sample[order(data_sample$when, data_sample$label),
               c("when", "label", "n", "Cumsum")]
  row.names(data_sample) <- NULL
  names(data_sample) <- c("Datum", "Beskrivning", "N", "Cumsum")
  
  return(data_sample)
}
