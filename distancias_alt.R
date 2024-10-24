library(tidyverse)
library(sf)
library(mapview)

# Ativadores de eixo
ativadores_eixo <- bind_rows(list(
  read_sf("dados/geradores-eixo/corredor-onibus/SIRGAS_SHP_corredoronibus_line.shp") |> 
    mutate(tipo = "onibus") |> select(tipo, geometry) |> st_set_crs("epsg:31983"),
  read_sf("dados/geradores-eixo/estacao-metro/SIRGAS_SHP_estacaometro_point.shp") |> 
    mutate(tipo = "metro") |> select(tipo, geometry) |> st_set_crs("epsg:31983"),
  read_sf("dados/geradores-eixo/estacao-trem/SIRGAS_SHP_estacaotrem.shp") |> 
    mutate(tipo = "trem") |> select(tipo, geometry) |> st_set_crs("epsg:31983") 
)) |> 
  group_by(tipo) |> 
  summarize(geometry = st_union(geometry))

PDE <- read_sf("dados/PDE/sirgas_PDE_3-Eixos-EETU.shp") |>
  st_set_crs("epsg:31983") 


mapview(ativadores_eixo |> st_buffer(1000)) + mapview(PDE)

