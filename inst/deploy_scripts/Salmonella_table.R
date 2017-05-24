library(svamap)
library(readxl)
##
##Read in the data from URAX
########################
## Read and parse creadentials
temp <- readLines("~/.smbcredentials")
user <- paste0("--user=", strsplit(temp[1], "=")[[1]][2])
pass <- paste0("--password=\"", strsplit(temp[2], "=")[[1]][2], "\"")
## Generate a temp loaction for the file
filename <- tempfile(fileext = ".xlsx")
## Get the file
system2("wget", args = c("--auth-no-challenge",
                         user,
                         pass,
                         "http://sharepointprod/sites/URAX/URAX%20storage/Salmonella_foder_2017_tabel%20066f08e2-4a0a-4866-a669-45ac8e161374.xlsx",
                         paste0("--output-document=", filename)
                         )
        )
## Read the file
df <- read_xlsx(filename)
## ## Tablulate the results
## df <- as.data.frame.matrix(table(df$Djurslag, df$Status))
## ## Create a variable
## df$Provtagna <- df$Neg + df$Pos + df$Pågåande
## ## Move the rownames into a variable
## df$Djurslag <- rownames(df)
## ## Order the columns neatly
## df <- df[,c("Djurslag", "Provtagna", "Pågåande", "Neg", "Pos")]
## ##df[,2:5] <- as.integer(0)
## ## write the table
## nolink <- c("<META NAME='ROBOTS' CONTENT='NOINDEX, NOFOLLOW'>")
## tab <- html_table(df,
##                   align = c("l", "l", "l", "l", "l"),
##                   html_head = generate_header(ordering =FALSE, otherstuff = nolink),
##                   footer = FALSE
##                   )
## ## Browse the table that we made
## ## browseURL(tab)
## ## The following lines will be used when we want to deploy the table to the web:
## ##
## ## Deploy map to Azure server. This is SVA's external website and is
## ## administered by a company "Episerver hosting" the contact for this
## ## company at SVA is the communications department.
## temp <- readLines("~/.svaftp_credentials")
## cred <- paste0("ftp://", temp[2], ":", temp[3], "@", temp[1], "/MAPS/Salmonella/")
## library(RCurl)
## ftpUpload(tab, paste0(cred, "table.html"))
