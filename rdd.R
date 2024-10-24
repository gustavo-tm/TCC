library(tidyverse)
library(rdrobust)
library(sf)

# RDPLOT ----

readRDS("cache/2010-distancias.RDS") |> 
  st_drop_geometry() |>
  mutate(densidade = moradores / as.numeric(area_postcut)) |> 
  drop_na() |>
  filter(abs(distancia) < 500, as.numeric(area_postcut) > 10) |>
  (\(df) rdplot(as.numeric(df$densidade), df$distancia, 
                binselect = "es", ci = .99, p = 1, nbins = c(5,5), c = 0))(df = _) 

readRDS("cache/2022-distancias.RDS") |> 
  st_drop_geometry() |> 
  mutate(densidade = moradores / as.numeric(area_postcut)) |> 
  drop_na() |>
  filter(abs(distancia) < 500, as.numeric(area_postcut) > 10) |>
  (\(df) rdplot(as.numeric(df$densidade), df$distancia, 
                binselect = "es", ci = .99, p = 1, nbins = c(5,5)))(df = _)

# RDROBUST -----


readRDS("cache/2010-distancias.RDS") |> 
  st_drop_geometry() |>
  mutate(densidade = moradores / as.numeric(area_postcut)) |> 
  drop_na() |>
  filter(abs(distancia) < 500, as.numeric(area_postcut) > 100) |>
  (\(df) rdrobust(as.numeric(df$densidade), df$distancia, 
                kernel = "tri", p = 1))(df = _) |> 
  summary()


readRDS("cache/2022-distancias.RDS") |> 
  st_drop_geometry() |> 
  mutate(densidade = moradores / as.numeric(area_postcut)) |> 
  drop_na() |>
  filter(abs(distancia) < 500, as.numeric(area_postcut) > 100) |> 
  (\(df) rdrobust(as.numeric(df$densidade), df$distancia, 
                  kernel = "tri", p = 1, c = 0))(df = _) |> 
  summary()




