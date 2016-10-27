library(sp)

# Load kommuner from svamap package and change encoding --> peraphs to fix
load(file = system.file("data/kommuner.rda", package = "svamap"))
Encoding(kommuner@data$KnNamn) <- "UTF-8"

# Load postnummer data from svar package
load(file = system.file("data/postnummer2015.rda", package = "svar"))
postnummer2015$POSTALCODE <- as.character(postnummer2015$POSTALCODE) 

# Load kvarka data
kvarka <- read.csv2(file = "T:/Falkenrapporter/E13-008 Grundrapport.csv",
                    header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8",
                    na.strings = c("NA", " ", ""))

# Select just interesting columns
kvarka_data_map <- data.frame(uppdrag = kvarka$Uppdragid,
                              status = kvarka$Status..numerisk.,
                              postort = kvarka$Kundort,
                              postnum = kvarka$Kundpostnr, 
                              date = kvarka$Ankomstdatum,
                              stringsAsFactors = FALSE)

kvarka_data_map$date <- as.Date(kvarka_data_map$date)

# Subset påvisad only samples
kvarka_data_map <- kvarka_data_map[kvarka_data_map$status == 1 &
                                     !is.na(kvarka_data_map$status), ]

# Subset samples in the period and create a new column 2 years - last 60 days (2) and last 60 days (1)
kvarka_data_map$period <- ifelse(kvarka_data_map$date >= Sys.Date()- 730 & kvarka_data_map$date < Sys.Date()- 60, 2,
                          ifelse(kvarka_data_map$date >= Sys.Date()- 60, 1, 0))

# Remove duplicated uppdrag
kvarka_data_map <- kvarka_data_map[!duplicated(kvarka_data_map$uppdrag), ]

kvarka_data_map$postnum <- sub(" ", "", kvarka_data_map$postnum)

if(!all(kvarka_data_map$postnum %in% postnummer2015@data$POSTALCODE)) {
  warning("The following postnummer is/are not in our postnummer database: ", paste0(
                 unique(kvarka_data_map$postnum[!kvarka_data_map$postnum %in% postnummer2015@data$POSTALCODE]),
                 collapse = ", "), ".\n They will not be displayed in the final map" )
}

# Drop records not present in the postnummer database
kvarka_data_map <- kvarka_data_map[kvarka_data_map$postnum %in% postnummer2015@data$POSTALCODE,]

# Assign each postnummer to a kommun
kvarka_data_map$kommun <- postnummer2015$MUNICIPALI[match(kvarka_data_map$postnum, postnummer2015@data$POSTALCODE)]

if(!all(nchar(kvarka_data_map$kommun) == 4)) {
  warning("The following postnummer encompass more than one kommun: ", paste0(
    unique(kvarka_data_map$postnum[nchar(kvarka_data_map$kommun) != 4]),
    collapse = ", "), ".\n They will not be displayed in the final map" )
}

# Drop records not reffering to a unique kommun
kvarka_data_map <- kvarka_data_map[nchar(kvarka_data_map$kommun) == 4, ]

# Count number of occurrences per kommun and period. Discarge those that have 0 occurrences
pavisad_final <- as.data.frame(table(kvarka_data_map$kommun, kvarka_data_map$period), stringsAsFactors = FALSE)
colnames(pavisad_final) <- c("kommun", "period", "count")
pavisad_final <- pavisad_final[pavisad_final$count != 0, ]

# Create a kolumn with the expected values to display in the choropleth map
pavisad_final$resultat <- ifelse(pavisad_final$period == 1 & pavisad_final$count == 1, 1,
                          ifelse(pavisad_final$period == 1 & pavisad_final$count == 2, 2,
                          ifelse(pavisad_final$period == 1 & pavisad_final$count >= 3, 3, 
                          ifelse(pavisad_final$period == 2 & pavisad_final$count >= 1, 4, 0))))

# Delete duplicates. Note that the function drops always the second duplicated record.
pavisad_final <- pavisad_final[order(pavisad_final$period),]
pavisad_final <- pavisad_final[!duplicated(pavisad_final$kommun), ]

# Create a spdf with just those kommuner with påvisad samples
kvarka_map <- kommuner[which(kommuner@data$KnKod %in% pavisad_final$kommun), ]
kvarka_map@data <- merge(kvarka_map@data, pavisad_final, by.x = "KnKod", by.y = "kommun")

# Change spdf reference system
kvarka_map <- spTransform(kvarka_map, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Create a string with the popup text to use in the map
kvarka_map$popup_text <- ifelse(kvarka_map@data$period == 1,
 
   paste("Påvisade kvarkabakterier senaste 60 dagarna",
                                  "<b>Kommun</b>", kvarka_map@data$KnNamn,
                                  "<b>Antal provtagningstillfällen:</b>", kvarka_map@data$resultat,
                                  sep = "<br/>"),
  
   paste("Kvarkabakterier påvisade under senaste 24 månaderna",
                                  "<b>Kommun</b>", kvarka_map@data$KnNamn,
                                  sep = "<br/>")
)