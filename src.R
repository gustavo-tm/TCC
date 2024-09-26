library(tidyverse)
library(sf)

# https://geosampa.prefeitura.sp.gov.br/
PDE <- read_sf("dados/PDE/sirgas_PDE_3-Eixos-EETU.shp") |>
  st_set_crs("epsg:31983") 

# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/26565-malhas-de-setores-censitarios-divisoes-intramunicipais.html?edicao=39501&t=acesso-ao-produto
censo2022 <- read_sf("dados/censo/2022/SP_Malha_Preliminar_2022.shp") |> 
  filter(CD_MUN == "3550308") |> 
  st_transform("epsg:31983")

# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/26565-malhas-de-setores-censitarios-divisoes-intramunicipais.html?edicao=26589&t=acesso-ao-produto
censo2010 <- read_sf("dados/censo/2010/35SEE250GC_SIR.shp") |> 
  filter(CD_GEOCODM == "3550308") |> 
  st_transform("epsg:31983")

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











