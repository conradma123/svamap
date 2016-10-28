library(svamap)
library(sp)
##
##Read in the point data
##
pts <- read_point_data("/media/t/Falkenrapporter/E16-036 Grundrapport.csv")
pts@data$Label <- factor(pts@data$Kön, levels = c("Hankön", "Honkön"))
levels(pts@data$Label)
point_map(pts,
          by = "Label",
          path_to_file = "~/Desktop/map.pdf",
          col = c("#F2A900", "#D22630"),
          legend = TRUE,
          cex = 6,
          legend_labs = c("Pos",
                          "Neg"),
          legend_position = c(1100405, 7532675),
          height = 25,
          width = 17.7,
          legcex = 3.5
          )
