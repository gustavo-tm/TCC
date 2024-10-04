library(tidyverse)
library(sf)
# remotes::install_github("coolbutuseless/ggpattern")

# Import dados ----

## PDE ----
PDE <- read_sf("dados/PDE/sirgas_PDE_3-Eixos-EETU.shp") |>
  st_set_crs("epsg:31983") 

PDE.buffersimples <- PDE |> 
  st_buffer(1000) |> 
  st_union() |> 
  st_simplify(dTolerance = 100)


gg <- ggplot() +
  geom_sf(data = PDE.buffersimples) +
  geom_sf(data = st_make_grid(PDE.buffersimples, 
                              what = "corners", 
                              square = T,
                              cellsize = 200) |> as_tibble() |> 
            filter(st_intersects(geometry, PDE.buffersimples) |> as.logical()), 
          alpha = .5, size = .1, aes(geometry = geometry))

ggsave("test.pdf", gg, width = 30, height = 40)



## Zoneamento ----

zoneamento <- list.files(path = "dados/zoneamento/", full.names = T) |> 
  (\(arquivos) arquivos[grepl(pattern = "\\.shp$", x = arquivos)])(arquivos = _) |> 
  (\(arquivos) arquivos[-3])(arquivos = _) |> # Tirar quadras públicas
  lapply(read_sf) |>
  bind_rows() |>
  st_set_crs("epsg:5533") |> 
  st_transform("epsg:31983") |> 
  st_simplify(dTolerance = 10) |> 
  filter(st_covered_by(geometry, PDE.buffersimples) |> as.logical()) |> 
  summarize(geometria = st_union(geometry))

gg <- ggplot(zoneamento) +
  geom_sf(data = PDE.buffersimples, color = NA, fill = "grey", alpha = .5) +
  geom_sf(color = NA, fill = "darkgrey") +
  geom_sf(data = PDE, fill = "black", color = NA) +
  theme_void()

ggsave("output/zoneamento_PDE.pdf", gg, width = 30, height = 40)
remove(gg)

## Censo 2022 ----

# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/26565-malhas-de-setores-censitarios-divisoes-intramunicipais.html?edicao=26589&t=acesso-ao-produto
censo2010 <- read_sf("dados/censo/2010/35SEE250GC_SIR.shp") |> 
  filter(CD_GEOCODM == "3550308") |> 
  st_transform("epsg:31983") |> 
  select(id_setor = CD_GEOCODI) |> 
  left_join(readxl::read_excel("dados/censo/2010/Basico_SP1.XLS") |> 
              select(id_setor = Cod_setor, moradores = V002) |> 
              mutate(id_setor = as.character(id_setor))) |> 
  filter(st_covered_by(geometry, PDE.buffersimples) |> as.logical())

censo2010.intersec <- censo2010 |> 
  st_intersection(zoneamento)

censo2010.cut <- censo2010.intersec |> 
  mutate(area_precut = st_area(geometry) |> ceiling()) |> 
  st_difference(PDE |> st_union()) |> 
  mutate(area_postcut = st_area(geometry)|> floor(),
         eixo_percent = as.numeric((area_precut - area_postcut) / area_precut)) |> 
  st_drop_geometry()

mapa2010.cut <- ggplot() +
  # geom_sf(data = censo2010.intersec |> 
  #                   left_join(censo2010.cut |> select(id_setor, eixo_percent)) |> 
  #                   mutate(eixo_percent = cut(eixo_percent, breaks = 0:10/10)),
  #                 aes(geometry = geometry, fill = eixo_percent), 
  #                 color = NA) +
  ggpattern::geom_sf_pattern(data = PDE |> st_simplify(dTolerance = 50), colour = NA, fill = "black", alpha = .75) +
  # geom_sf(data = PDE |> st_simplify(dTolerance = 50), colour = NA, fill = "black", alpha = .75) +
  theme_void() +
  scale_fill_viridis_d()

ggsave("output/mapa_percent_eixo.pdf", mapa2010.cut, width = 3, height = 4)

mapa2010.cut <- ggplot() +
  ggpattern::geom_sf_pattern(data = censo2010.intersec |> 
                               left_join(censo2010.cut |> select(id_setor, eixo_percent)) |> 
                               mutate(eixo_percent = cut(eixo_percent, breaks = 0:10/10)),
                             aes(geometry = geometry, fill = eixo_percent), 
                             color = NA, pattern = "circle") +
  geom_sf(data = PDE, colour = "red", fill = NA, alpha = .5) +
  theme_void() +
  scale_fill_viridis_d()

ggsave("output/mapa_percent_eixo.pdf", mapa2010.cut, width = 30, height = 40)




