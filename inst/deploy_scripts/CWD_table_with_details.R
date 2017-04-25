library(svamap)
library(rgeos)
library(sp)
library(RCurl)
##
data(rough_lan)
##
##Read in the data
########################
pts <- read.csv2("/media/t/Falkenrapporter/E16-036 Grundrapport.csv")
## This is the data that should be tablulated
pts <- read_point_data("/media/t/Falkenrapporter/E16-036 Grundrapport.csv")
##
pts@data$Publicera <- factor(pts@data$Publicera, levels = c("Ja", "Nej"))
##
##Now keep all negatives unless Publicera is "Nej"; Drop all Positives unless Publicera is "Ja"
##
pts <- pts[(pts@data$Status..numerisk. == 0 &
            (pts@data$Publicera != "Nej" | is.na(pts@data$Publicera))
           ) |
           (pts@data$Publicera == "Ja" & !is.na(pts@data$Publicera)),]
## Drop the points that are not "Vilt (Jakt - fiske - natur)"
########################
pts <- pts[pts$Djurhållning == "Vilt (Jakt - fiske - natur)" & !is.na(pts$Djurhållning),]
##
########################
##
##Count points per polygon and species
########################
species_lists <- lapply(unique(pts@data$Djurslag), function(x){
    polys <- svamap::match_to_county(pts[pts@data$Djurslag == x,],
                            rough_lan,
                            "NUTS_ID")
    polys <- polys[[1]]
    ## Just keep the basic info for the table
    df <- polys@data[,c("name", "count")]
    df$count <- as.integer(df$count)
    df$count[is.na(df$count)] <- 0
    df$djurslag <- x
    return(df)
})
polys <- svamap::match_to_county(pts, rough_lan, "NUTS_ID")
polys <- polys[[1]]
## Just keep the basic info for the table
df <- polys@data[,c("name", "count")]
df$count <- as.integer(df$count)
df$count[is.na(df$count)] <- 0
for(i in seq_len(length(species_lists))) {
    df[,unique(pts@data$Djurslag)[i]] <- species_lists[[i]]$count
}
total <- unlist(lapply(c("count", unique(pts@data$Djurslag)), function(x){
    sum(df[,x])
}))
df <- rbind(df, c("Total", total))
## write the table
nolink <- c("<META NAME='ROBOTS' CONTENT='NOINDEX, NOFOLLOW'>")
tab <- html_table(df,
                  align = c("l", "r", rep("r", length(unique(pts@data$Djurslag)))),
                  col.names = c("Län", "Antal undersökta", unique(pts@data$Djurslag)),
                  html_head = generate_header(ordering =TRUE, otherstuff = nolink),
                  footer = TRUE
                  )
## Deploy map to Azure server. This is SVA's external website and is
## administered by a company "Episerver hosting" the contact for this
## company at SVA is the communications department.
temp <- readLines("~/.svaftp_credentials")
cred <- paste0("ftp://", temp[2], ":", temp[3], "@", temp[1], "/MAPS/CWD_species_table/")
ftpUpload(tab, paste0(cred, "table.html"))
