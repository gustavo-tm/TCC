library(tidyverse)
library(sf)

# Import dados ----

# https://geosampa.prefeitura.sp.gov.br/
PDE <- read_sf("dados/PDE/sirgas_PDE_3-Eixos-EETU.shp") |>
  st_set_crs("epsg:31983") 

# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/26565-malhas-de-setores-censitarios-divisoes-intramunicipais.html?edicao=39501&t=acesso-ao-produto
censo2022 <- read_sf("dados/censo/2022/SP_Malha_Preliminar_2022.shp") |> 
  filter(CD_MUN == "3550308") |> 
  st_transform("epsg:31983") |> 
  select(id_setor = CD_SETOR)

# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/26565-malhas-de-setores-censitarios-divisoes-intramunicipais.html?edicao=26589&t=acesso-ao-produto
censo2010 <- read_sf("dados/censo/2010/35SEE250GC_SIR.shp") |> 
  filter(CD_GEOCODM == "3550308") |> 
  st_transform("epsg:31983") |> 
  select(id_setor = CD_GEOCODI)

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



