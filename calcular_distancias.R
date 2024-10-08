library(tidyverse)
library(sf)

# Import dados ----

## PDE ----
PDE <- read_sf("dados/PDE/sirgas_PDE_3-Eixos-EETU.shp") |>
  st_set_crs("epsg:31983") 

PDE.buffersimples <- PDE |> 
  st_buffer(800) |> 
  st_union() |> 
  st_simplify(dTolerance = 100)


# gg <- ggplot() +
#   geom_sf(data = PDE.buffersimples) +
#   geom_sf(data = st_make_grid(PDE.buffersimples, 
#                               what = "corners", 
#                               square = T,
#                               cellsize = 200) |> as_tibble() |> 
#             filter(st_intersects(geometry, PDE.buffersimples) |> as.logical()), 
#           alpha = .5, size = .1, aes(geometry = geometry))
# 
# ggsave("test.pdf", gg, width = 30, height = 40)



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



prepara.dados <- function(dados, ano){
  dados.intersec <- dados |> 
    st_intersection(zoneamento)
  print("Setor censitário cortado no formato dos lotes")
  
  dados.cut <- dados.intersec |> 
    mutate(area_precut = st_area(geometry) |> ceiling()) |> 
    st_difference(PDE |> st_union()) |> 
    mutate(area_postcut = st_area(geometry)|> floor(),
           eixo_percent = as.numeric((area_precut - area_postcut) / area_precut)) |> 
    st_drop_geometry()
  print("Percentual do setor censitário coberto por eixo calculado")
  
  mapa.cut <- ggplot() +
    geom_sf(data = dados.intersec |>
              left_join(dados.cut |> select(id_setor, eixo_percent)) |>
              mutate(eixo_percent = cut(eixo_percent, breaks = 0:5/5)),
            aes(geometry = geometry, fill = eixo_percent),
            color = NA) +
    geom_sf(data = dados, fill = NA, colour = "#313638", alpha = .3) +
    geom_sf(data = st_make_grid(PDE |> st_union(), 
                                what = "corners", 
                                square = T,
                                cellsize = 30) |> as_tibble() |> 
              filter(st_intersects(geometry, PDE |> st_union()) |> as.logical()),
            aes(geometry = geometry), size = .25, stroke = 0) + #AREA HACHURADA
    theme_void() +
    scale_fill_brewer()
  
  ggsave(str_glue("output/{ano}mapa_percent_eixo.pdf"), mapa.cut, width = 30, height = 40)
  
  mapa.8020 <- ggplot() +
    geom_sf(data = dados.intersec |> 
              left_join(dados.cut |> select(id_setor, eixo_percent)) |> 
              mutate(situacao = case_when(eixo_percent <= .2 ~ "Não Eixo",
                                          eixo_percent >= .8 ~ "Eixo",
                                          TRUE ~ "Fora da análise")),
            aes(geometry = geometry, fill = situacao), color = NA) +
    geom_sf(data = dados, colour = "#313638", fill = NA, alpha = .5) +
    geom_sf(data = st_make_grid(PDE |> st_union(), 
                                what = "corners", 
                                square = T,
                                cellsize = 30) |> as_tibble() |> 
              filter(st_intersects(geometry, PDE |> st_union()) |> as.logical()),
            aes(geometry = geometry), size = .25, stroke = 0) + #AREA HACHURADA
    scale_fill_manual(values = c("Não Eixo" = "#68BBFF",
                                 "Eixo" = "#0051A1",
                                 "Fora da análise" = "#B21F00")) +
    theme_void()
  ggsave(str_glue("output/{ano}mapa-cut8020.pdf"), mapa.8020, width = 30, height = 40)
  
  # Construção da running variable ----
  
  dados.grupos <-  dados.cut |> 
    mutate(grupo = case_when(eixo_percent <= .2 ~ "Controle",
                             eixo_percent >= .8  ~ "Tratamento",
                             TRUE               ~ "Fora")) |> 
    left_join(dados) |> st_as_sf() |> 
    mutate(centroide = st_centroid(geometry))
  print("Controle e tratamento definidos")
  
  geometrias.controle <- dados.grupos |> 
    filter(grupo == "Controle") |> 
    select(geometry, centroide)
  
  geometrias.tratamento <- dados.grupos |> 
    filter(grupo == "Tratamento") |> 
    select(geometry, centroide)
  
  dados.distancias <- bind_rows(
    #Distância entre cada unidade de controle a mais próxima do tratamento
    dados.grupos |> 
      filter(grupo == "Controle") |> 
      rowwise() |> 
      mutate(distancia_centroide = -as.numeric(st_distance(geometry, geometrias.tratamento$centroide[st_nearest_feature(geometry, geometrias.tratamento$centroide)])[1]),
             distancia_geometria = -as.numeric(st_distance(geometry, geometrias.tratamento$geometry[st_nearest_feature(geometry, geometrias.tratamento$geometry)])[1])),
    dados.grupos |> 
      filter(grupo == "Tratamento") |> 
      rowwise() |> 
      mutate(distancia_centroide = as.numeric(st_distance(geometry, geometrias.controle$centroide[st_nearest_feature(geometry, geometrias.controle$centroide)])[1]),
             distancia_geometria = as.numeric(st_distance(geometry, geometrias.controle$geometry[st_nearest_feature(geometry, geometrias.controle$geometry)])[1]))
  )
  print("Distâncias calculadas")
  
  # 
  # dados.distancias |>
  #   ggplot(aes(x = distancia, y = moradores, color = grupo)) +
  #   geom_point(alpha = .025) +
  #   geom_smooth() +
  #   geom_vline(xintercept = 0, linetype = "dashed") +
  #   xlim(c(-500, 500)) +
  #   theme_classic()
  # 
  # ggsave(str_glue("output/{ano}rdd.pdf"), width = 8, height = 5)
  
  return(dados.distancias)
}

# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/26565-malhas-de-setores-censitarios-divisoes-intramunicipais.html?edicao=26589&t=acesso-ao-produto
censo2010 <- read_sf("dados/censo/2010/35SEE250GC_SIR.shp") |> 
  filter(CD_GEOCODM == "3550308") |> 
  st_transform("epsg:31983") |> 
  select(id_setor = CD_GEOCODI) |> 
  left_join(readxl::read_excel("dados/censo/2010/Basico_SP1.XLS") |> 
              select(id_setor = Cod_setor, moradores = V002) |> 
              mutate(id_setor = as.character(id_setor))) |> 
  filter(st_covered_by(geometry, PDE.buffersimples) |> as.logical())

distancias.2010 <- prepara.dados(censo2010, 2010)
remove(censo2010)
gc()

censo2022 <- read_sf("dados/censo/2022/SP_Malha_Preliminar_2022.shp") |> 
  filter(CD_MUN == "3550308") |> 
  st_transform("epsg:31983")  |>
  select(id_setor = CD_SETOR, moradores = v0001) |> 
  filter(st_covered_by(geometry, PDE.buffersimples) |> as.logical()) # Apenas valores perto dos EETUs

distancias.2022 <- prepara.dados(censo2022, 2022)
remove(censo2022)
gc()
