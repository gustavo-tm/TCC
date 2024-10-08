library(mapview)
library(dplyr)
library(sf)

# Join and mutate as in the original ggplot code
censo2010_intersec <- censo2010.intersec %>%
  left_join(select(censo2010.cut, id_setor, eixo_percent)) %>%
  mutate(situacao = case_when(
    eixo_percent <= 0.2 ~ "Não Eixo",
    eixo_percent >= 0.8 ~ "Eixo",
    TRUE ~ "Fora da análise"
  ))

# Main mapview layer with situacao coloring
map <- mapview(censo2010_intersec, zcol = "situacao", 
               col.regions = c("Não Eixo" = "#68BBFF", 
                               "Eixo" = "#0051A1", 
                               "Fora da análise" = "#B21F00"),
               legend = TRUE)

# Adding the boundary layer (equivalent to geom_sf in ggplot2)
boundary <- mapview(censo2010, color = "#313638", alpha.regions = 0)

# Create grid and overlay (equivalent to the hatched area in ggplot2)
grid <- st_make_grid(st_union(PDE), square = TRUE, cellsize = 30) %>%
  st_intersection(st_union(PDE))

# Add the grid layer
grid_layer <- mapview(grid, color = "#000000", alpha.regions = 0)

# Combine layers
final_map <- map + boundary + grid_layer

# Display the map
final_map
