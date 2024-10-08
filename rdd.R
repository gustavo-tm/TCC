library(tidyverse)
library(rdrobust)


distancias.2010 |> 
  st_drop_geometry() |> 
  filter(abs(distancia_geometria) < 200) |> 
  (\(df) rdplot(df$moradores, df$distancia_geometria, ci = .99)$rdplot + ylim(c(0,1000)))(df = _) 

distancias.2022 |> 
  st_drop_geometry() |> 
  filter(abs(distancia_geometria) < 200) |> 
  (\(df) rdplot(df$moradores, df$distancia_geometria, ci = .95)$rdplot + ylim(c(0,1000)))(df = _)


distancias.2010 |> 
  st_drop_geometry() |> 
  filter(abs(distancia_geometria) < 200) |> 
  (\(df) rdplot(as.numeric(df$area_precut), df$distancia_centroide, ci = .99)$rdplot)(df = _) 

distancias.2022 |> 
  st_drop_geometry() |> 
  filter(abs(distancia_geometria) < 200) |> 
  (\(df) rdplot(as.numeric(df$area_precut), df$distancia_centroide, ci = .95)$rdplot)(df = _)
