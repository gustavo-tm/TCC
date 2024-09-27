library(tidyverse)
library(sf)

# Import dados ----

# https://geosampa.prefeitura.sp.gov.br/
PDE <- read_sf("dados/PDE/sirgas_PDE_3-Eixos-EETU.shp") |>
  st_set_crs("epsg:31983") 

censo2022 <- read_sf("dados/censo/2022/SP_Malha_Preliminar_2022.shp") |> 
  filter(CD_MUN == "3550308") |> 
  st_transform("epsg:31983")  |>
  select(id_setor = CD_SETOR, moradores = v0001)


# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/26565-malhas-de-setores-censitarios-divisoes-intramunicipais.html?edicao=26589&t=acesso-ao-produto
censo2010 <- read_sf("dados/censo/2010/35SEE250GC_SIR.shp") |> 
  filter(CD_GEOCODM == "3550308") |> 
  st_transform("epsg:31983") |> 
  select(id_setor = CD_GEOCODI) |> 
  left_join(readxl::read_excel("dados/censo/2010/Basico_SP1.XLS") |> 
              select(id_setor = Cod_setor, moradores = V002) |> 
              mutate(id_setor = as.character(id_setor)))


## Visualização mapas ----
mapa2010 <- ggplot() +
  geom_sf(data = censo2010) +
  geom_sf(data = PDE, colour = NA, fill = "darkred", alpha = .5) +
  theme_void()

mapa2022 <- ggplot() +
  geom_sf(data = censo2022) +
  geom_sf(data = PDE, colour = NA, fill = "darkblue", alpha = .5) +
  theme_void()

ggsave("output/mapa2010.pdf", mapa2010, width = 30, height = 40)
ggsave("output/mapa2022.pdf", mapa2022, width = 30, height = 40)

# Definição de corte ----

## 2022 ----
censo2022.cut <- censo2022 |> 
  mutate(area_precut = st_area(geometry) |> ceiling()) |> 
  st_difference(PDE |> st_union()) |> 
  mutate(area_postcut = st_area(geometry)|> floor(),
         eixo_percent = as.numeric((area_precut - area_postcut) / area_precut)) |> 
  st_drop_geometry()



mapa2022.cut <- ggplot() +
  geom_sf(data = censo2022 |> 
            left_join(censo2022.cut |> select(id_setor, eixo_percent)) |> 
            mutate(eixo_percent = cut(eixo_percent, breaks = 0:10/10)),
          aes(geometry = geometry, fill = eixo_percent), color = NA) +
  geom_sf(data = PDE, colour = "white", fill = NA, alpha = .5) +
  theme_void() +
  scale_fill_viridis_d()
ggsave("output/mapa2022-cut.pdf", mapa2022.cut, width = 30, height = 40)


gg <- censo2022.cut |>
  mutate(join = 1) |> 
  left_join(tibble(cut = 1:100/100,
                   join = 1)) |> 
  mutate(eixo = eixo_percent >= cut,
         nao_eixo = eixo_percent < cut) |> 
  group_by(cut) |> 
  summarize(eixo = sum(eixo),
            nao_eixo = sum(nao_eixo)) |> 
  pivot_longer(eixo:nao_eixo) |> 
  ggplot(aes(x = cut, y = value, fill = reorder(name, -value))) +
  geom_col(width = 1/100) +
  theme_classic() +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_viridis_d(labels = c("nao_eixo" = "Fora dos Eixos",
                                  "eixo" = "Nos Eixos"),
                       name = "") +
  labs(x = "Definição de corte do percentual da área de intersecção",
       y = "Número de setores censitários")
ggsave("output/proporcao2022-cut.pdf", gg, width = 6, height = 4)

## 2010 ----
censo2010.cut <- censo2010 |> 
  mutate(area_precut = st_area(geometry) |> ceiling()) |> 
  st_difference(PDE |> st_union()) |> 
  mutate(area_postcut = st_area(geometry)|> floor(),
         eixo_percent = as.numeric((area_precut - area_postcut) / area_precut)) |> 
  st_drop_geometry()



mapa2010.cut <- ggplot() +
  geom_sf(data = censo2010 |> 
            left_join(censo2010.cut |> select(id_setor, eixo_percent)) |> 
            mutate(eixo_percent = cut(eixo_percent, breaks = 0:10/10)),
          aes(geometry = geometry, fill = eixo_percent), color = NA) +
  geom_sf(data = PDE, colour = "white", fill = NA, alpha = .5) +
  theme_void() +
  scale_fill_viridis_d()
ggsave("output/mapa2010-cut.pdf", mapa2010.cut, width = 30, height = 40)


gg <- censo2010.cut |>
  mutate(join = 1) |> 
  left_join(tibble(cut = 1:100/100,
                   join = 1)) |> 
  mutate(eixo = eixo_percent >= cut,
         nao_eixo = eixo_percent < cut) |> 
  group_by(cut) |> 
  summarize(eixo = sum(eixo),
            nao_eixo = sum(nao_eixo)) |> 
  pivot_longer(eixo:nao_eixo) |> 
  ggplot(aes(x = cut, y = value, fill = reorder(name, -value))) +
  geom_col(width = 1/100) +
  theme_classic() +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_viridis_d(labels = c("nao_eixo" = "Fora dos Eixos",
                                  "eixo" = "Nos Eixos"),
                       name = "") +
  labs(x = "Definição de corte do percentual da área de intersecção",
       y = "Número de setores censitários")
ggsave("output/proporcao2010-cut.pdf", gg, width = 6, height = 4)

# Teste visual diferentes cortes----

## 2022 80-20 ----

mapa2022.8020 <- ggplot() +
  geom_sf(data = censo2022 |> 
            left_join(censo2022.cut |> select(id_setor, eixo_percent)) |> 
            mutate(situacao = case_when(eixo_percent <= .2 ~ "Não Eixo",
                                        eixo_percent >= .8 ~ "Eixo",
                                        TRUE ~ "Fora da análise")),
          aes(geometry = geometry, fill = situacao), color = NA) +
  geom_sf(data = PDE, colour = "white", fill = NA, alpha = .5) +
  scale_fill_manual(values = c("Não Eixo" = "lightblue",
                               "Eixo" = "darkgreen",
                               "Fora da análise" = "red")) +
  theme_void()
ggsave("output/mapa2022-cut8020.pdf", mapa2022.8020, width = 30, height = 40)

## 2022 60-15 ----

mapa2022.6015 <- ggplot() +
  geom_sf(data = censo2022 |> 
            left_join(censo2022.cut |> select(id_setor, eixo_percent)) |> 
            mutate(situacao = case_when(eixo_percent <= .15 ~ "Não Eixo",
                                        eixo_percent >= .6 ~ "Eixo",
                                        TRUE ~ "Fora da análise")),
          aes(geometry = geometry, fill = situacao), 
          color = "darkgrey", lwd = .1) +
  geom_sf(data = PDE, colour = "white", fill = NA, alpha = .5) +
  scale_fill_manual(values = c("Não Eixo" = "lightblue",
                               "Eixo" = "darkgreen",
                               "Fora da análise" = "red")) +
  theme_void()
ggsave("output/mapa2022-cut6015.pdf", mapa2022.6015, width = 30, height = 40)

## 2010 80-20 ----

mapa2010.8020 <- ggplot() +
  geom_sf(data = censo2022 |> 
            left_join(censo2010.cut |> select(id_setor, eixo_percent)) |> 
            mutate(situacao = case_when(eixo_percent <= .2 ~ "Não Eixo",
                                        eixo_percent >= .8 ~ "Eixo",
                                        TRUE ~ "Fora da análise")),
          aes(geometry = geometry, fill = situacao), color = NA) +
  geom_sf(data = PDE, colour = "white", fill = NA, alpha = .5) +
  scale_fill_manual(values = c("Não Eixo" = "lightblue",
                               "Eixo" = "darkgreen",
                               "Fora da análise" = "red")) +
  theme_void()
ggsave("output/mapa2010-cut8020.pdf", mapa2010.8020, width = 30, height = 40)

## 2022 60-15 ----

mapa2010.6015 <- ggplot() +
  geom_sf(data = censo2010 |> 
            left_join(censo2010.cut |> select(id_setor, eixo_percent)) |> 
            mutate(situacao = case_when(eixo_percent <= .15 ~ "Não Eixo",
                                        eixo_percent >= .6 ~ "Eixo",
                                        TRUE ~ "Fora da análise")),
          aes(geometry = geometry, fill = situacao), 
          color = "darkgrey", lwd = .1) +
  geom_sf(data = PDE, colour = "white", fill = NA, alpha = .5) +
  scale_fill_manual(values = c("Não Eixo" = "lightblue",
                               "Eixo" = "darkgreen",
                               "Fora da análise" = "red")) +
  theme_void()
ggsave("output/mapa2010-cut6015.pdf", mapa2010.6015, width = 30, height = 40)


# Construção da running variable ----

## 2010 ----
censo2022.grupos <-  censo2022.cut |> 
  mutate(grupo = case_when(eixo_percent < .15 ~ "Controle",
                           eixo_percent > .6  ~ "Tratamento",
                           TRUE               ~ "Fora")) |> 
  left_join(censo2022 |> st_centroid()) |> st_as_sf() |> 
  filter(st_distance(geometry, st_union(PDE) |> st_simplify()) |> as.numeric() < 1000)

geometrias.controle <- censo2022.grupos |> 
  filter(grupo == "Controle") |> 
  pull(geometry)

geometrias.tratamento <- censo2022.grupos |> 
  filter(grupo == "Tratamento") |> 
  pull(geometry)

censo2022.distancias <- bind_rows(
  #Distância entre cada unidade de controle a mais próxima do tratamento
  censo2022.grupos |> 
    filter(grupo == "Controle") |> 
    rowwise() |> 
    mutate(distancia = st_distance(geometry, geometrias.tratamento[st_nearest_feature(geometry, geometrias.tratamento)])[1]),
  censo2022.grupos |> 
    filter(grupo == "Tratamento") |> 
    rowwise() |> 
    mutate(distancia = st_distance(geometry, geometrias.controle[st_nearest_feature(geometry, geometrias.controle)])[1])
)


censo2022.distancias |> 
  mutate(distancia = ifelse(grupo == "Controle", -as.numeric(distancia), as.numeric(distancia))) |> 
  ggplot(aes(x = distancia, y = moradores, color = grupo)) +
  geom_point(alpha = .025) +
  geom_smooth(method = "lm") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(c(-1000, 1000)) +
  theme_classic()

censo2022.distancias |> 
  st_drop_geometry() |> 
  mutate(distancia = ifelse(grupo == "Controle", -as.numeric(distancia), as.numeric(distancia)) |> 
           cut(breaks = c(-Inf, -(0:10*10), (1:10*10), Inf)) |> as.factor() |> as.numeric()- 10) |> 
  group_by(distancia, grupo) |> 
  summarize(moradores = mean(moradores)) |> 
  ggplot(aes(x = distancia, y = moradores, color = grupo)) +
  geom_point(alpha = 1) +
  geom_smooth(method = "lm") +
  # geom_vline(xintercept = 0, linetype = "dashed") +
  # xlim(c(-1000, 1000)) +
  theme_classic()

ggsave("output/rdd2022.pdf", width = 8, height = 5)  

## 2010 ----
censo2010.grupos <-  censo2010.cut |> 
  mutate(grupo = case_when(eixo_percent < .15 ~ "Controle",
                           eixo_percent > .6  ~ "Tratamento",
                           TRUE               ~ "Fora")) |> 
  left_join(censo2010 |> st_centroid()) |> st_as_sf() |> 
  filter(st_distance(geometry, st_union(PDE) |> st_simplify()) |> as.numeric() < 1000)

geometrias.controle <- censo2010.grupos |> 
  filter(grupo == "Controle") |> 
  pull(geometry)

geometrias.tratamento <- censo2010.grupos |> 
  filter(grupo == "Tratamento") |> 
  pull(geometry)

censo2010.distancias <- bind_rows(
  #Distância entre cada unidade de controle a mais próxima do tratamento
  censo2010.grupos |> 
    filter(grupo == "Controle") |> 
    rowwise() |> 
    mutate(distancia = st_distance(geometry, geometrias.tratamento[st_nearest_feature(geometry, geometrias.tratamento)])[1]),
  censo2010.grupos |> 
    filter(grupo == "Tratamento") |> 
    rowwise() |> 
    mutate(distancia = st_distance(geometry, geometrias.controle[st_nearest_feature(geometry, geometrias.controle)])[1])
)


censo2010.distancias |> 
  mutate(distancia = ifelse(grupo == "Controle", -as.numeric(distancia), as.numeric(distancia))) |> 
  ggplot(aes(x = distancia, y = moradores, color = grupo)) +
  geom_point(alpha = .025) +
  geom_smooth(method = "lm") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(c(-1000, 1000)) +
  theme_classic()

ggsave("output/rdd2010.pdf", width = 8, height = 5)  

