library(svamap)
data(NUTS_20M)
pts <- read_point_data()
polys <- svamap::match_to_county(pts, NUTS_20M, "NUTS_ID")
path_to_data <- svamap::write_data(polys)
svamap::write_page(path_to_data)

