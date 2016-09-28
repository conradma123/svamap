library(svamap)

## Test 1
pts <- read_point_data()
rm(list = ls())

## Test 2
temp <- read.csv2(system.file("sample_data_cwd.csv", package = "svamap"))
temp$Gisx[2] <- NA
path <- tempfile()
write.csv2(temp, file = path)
res <- tools::assertWarning(
    pts <- read_point_data(path))
stopifnot(length(grep("1 of the submitted points are missing coordinates",
                     res[[1]]$message)) > 0)
rm(list = ls())
