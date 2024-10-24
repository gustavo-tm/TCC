library(tidyverse)
library(rdrobust)
library(sf)

# RDPLOT ----

readRDS("cache/2010-distancias.RDS") |> 
  st_drop_geometry() |>
  mutate(densidade = moradores / as.numeric(area_postcut)) |> 
  drop_na() |>
  filter(abs(distancia_geometria) < 500, as.numeric(area_postcut) > 10) |>
  (\(df) rdplot(as.numeric(df$densidade), df$distancia_geometria, 
                binselect = "es", ci = .99, p = 1, nbins = c(5,5)))(df = _) 

readRDS("cache/2022-distancias.RDS") |> 
  st_drop_geometry() |> 
  mutate(densidade = moradores / as.numeric(area_postcut)) |> 
  drop_na() |>
  filter(abs(distancia_geometria) < 500, as.numeric(area_postcut) > 10) |>
  (\(df) rdplot(as.numeric(df$densidade), df$distancia_geometria, 
                binselect = "es", ci = .99, p = 1, nbins = c(5,5)))(df = _)

# RDROBUST -----


readRDS("cache/2010-distancias.RDS") |> 
  st_drop_geometry() |>
  mutate(densidade = moradores / as.numeric(area_postcut)) |> 
  drop_na() |>
  filter(abs(distancia_geometria) < 500, as.numeric(area_postcut) > 100) |>
  (\(df) rdrobust(as.numeric(df$densidade), df$distancia_geometria, 
                kernel = "tri", p = 1))(df = _) |> 
  summary()


readRDS("cache/2022-distancias.RDS") |> 
  st_drop_geometry() |> 
  mutate(densidade = moradores / as.numeric(area_postcut)) |> 
  drop_na() |>
  filter(abs(distancia_geometria) < 500, as.numeric(area_postcut) > 100) |> 
  (\(df) rdrobust(as.numeric(df$densidade), df$distancia_geometria, 
                  kernel = "tri", p = 1, c = 0))(df = _) |> 
  summary()



distancias.2022 |> 
  filter(abs(distancia_centroide) <= 43,
         as.numeric(area_postcut) > 100) |> 
  st_drop_geometry() |> 
  group_by(grupo) |> 
  summarize(n = n())


