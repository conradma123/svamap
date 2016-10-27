library(svamap)
library(sp)
##
##Read in the point data
pts <- read_point_data("/media/t/Falkenrapporter/E16-036 Grundrapport.csv")
pts@data$Könkod <- factor(pts@data$Könkod, levels = c("1", "2"))
point_map(pts, path_to_file = "~/Desktop/map.pdf")
point_map(pts,
          by = "Könkod",
          path_to_file = "~/Desktop/map.pdf",
          col = c("#999999", "#111111"),
          border_col = "grey50",
          cex = 0.6,
          legend = TRUE)
