library(mapview)
library(dplyr)
library(sf)


# Main mapview layer with situacao coloring
map <- mapview(readRDS("cache/2022grupos.RDS"), zcol = "grupo")
map.pde <- mapview(PDE, col.regions = "black")

(map + map.pde) |> 
  mapshot(url = "output/mapview/view.html")
