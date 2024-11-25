library(ggbump)
library(paletteer)

distrito <- read_sf("dados/distrito/SIRGAS_SHP_distrito.shp") |> 
  st_set_crs("epsg:31983")
censo <- readRDS("rdata/censo2010.rds")
IPTU <- readRDS("rdata/IPTU-lote.rds")

distrito.IPTU <- distrito |> 
  st_join(IPTU |> 
            filter(uso_predominante == "residencial") |> 
            mutate(geometry = st_centroid(geometry))) |> 
  st_drop_geometry() |> 
  group_by(ds_nome) |> 
  summarize(across(c(area_construida, area_terreno, area_ocupada, unidades), ~ sum(.x, na.rm = TRUE))) |> 
  mutate(verticalizacao = area_construida / area_ocupada)


distrito.censo <- distrito |> 
  st_join(censo |> 
            mutate(geometry = st_centroid(geometry))) |> 
  st_drop_geometry() |> 
  group_by(ds_nome) |> 
  summarize(across(c(moradores, domicilios), ~ sum(.x, na.rm = TRUE)))


df <- distrito |> 
  group_by(ds_nome) |> 
  summarize(geometry = st_union(geometry),
            area = sum(ds_areakm)) |> 
  left_join(distrito.censo) |> 
  left_join(distrito.IPTU) |> 
  mutate(densidade_habitacional = domicilios / area_terreno,
         densidade_construtiva = area_construida / area_terreno,
         espectro_irregularidade = unidades / (unidades + domicilios),
         densidade_residencial = moradores / area_terreno,
         densidade = moradores / area)



df |> 
  drop_na() |> 
  filter(row_number(abs(espectro_irregularidade - .5)) <= 40) |>
  mutate(across(c(verticalizacao, densidade_construtiva, densidade_habitacional, densidade, densidade_residencial), ~ row_number(-.x)),
         distancia = abs(densidade_residencial - verticalizacao),
         selected = ifelse(row_number(-distancia) <= 5, ds_nome, NA),
         alpha = ifelse(row_number(-distancia) <= 5, 1, 0)) |> 
  pivot_longer(c(verticalizacao, densidade_construtiva, densidade_habitacional, densidade_residencial)) |> 
  mutate(
         label = ifelse(name == "verticalizacao", ds_nome, NA) |> str_to_title()) |> 
  ggplot(aes(x = name, y = value, group = ds_nome)) +
  geom_bump(colour = "gray90", linewidth = 0.8, smooth = 6) +
  geom_bump(aes(colour = selected, alpha = alpha), linewidth = 0.8, smooth = 6) +
  geom_point(colour = "white", size = 4) +
  geom_point(colour = "gray90", size = 2) +
  geom_point(aes(colour = selected, alpha = alpha), size = 2) +
  geom_text(aes(label = label), x = 4.1, hjust = 0, color = "gray50") +
  scale_colour_paletteer_d("fishualize::Balistapus_undulatus") +
  scale_y_reverse("", breaks = 1:40) +  
  scale_x_discrete("", limits = c("densidade_residencial", 
                                  "densidade_habitacional",
                                  "densidade_construtiva",
                                  "verticalizacao"),
                   labels = c("Densidade\n populacional", "Densidade\n habitacional", "Densidade\n construtiva", "Verticalização"),
                   expand = expansion(add = c(0.2,1.1))) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = 14, hjust = .5),
        plot.subtitle = element_text(size = 10, hjust = .5),
        plot.caption = element_text(size = 8)) +
  labs(title = toupper("Verticalização não é densidade"))

ggsave("tex/figuras/bairros.pdf", width = 10, height = 7)
