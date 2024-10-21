library(tidyverse)
library(rdrobust)
library(sf)

distancias.2022 |> 
  filter(abs(distancia_geometria) < 100) |> 
  summarize(controle = sum(distancia_centroide < 0),
            tratamento = sum(distancia_centroide > 0))



rdd.2010 <- distancias.2010 |> 
  st_drop_geometry() |>
  mutate(densidade = moradores / as.numeric(area_postcut)) |> 
  drop_na() |>
  filter(abs(distancia_geometria) < 500, as.numeric(area_postcut) > 10) |>
  (\(df) rdplot(as.numeric(df$densidade), df$distancia_geometria, 
                binselect = "es", nbins = c(10, 10), ci = .99))(df = _) 
rdd.2010
rdd.2010$rdplot

rdd.2022 <- distancias.2022 |> 
  st_drop_geometry() |> 
  mutate(densidade = moradores / as.numeric(area_postcut)) |> 
  drop_na() |>
  filter(abs(distancia_geometria) < 500, as.numeric(area_postcut) > 10) |>
  (\(df) rdplot(as.numeric(df$densidade), df$distancia_geometria, 
                binselect = "es", nbins = c(10, 10), ci = .99))(df = _)

rdd.2022
rdd.2022$rdplot


