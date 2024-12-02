library(ggbump)
library(paletteer)

distrito <- read_sf("dados/distrito/SIRGAS_SHP_distrito.shp") |> 
  st_set_crs("epsg:31983")
# censo <- readRDS("rdata/censo2010.rds")
# IPTU <- readRDS("rdata/IPTU-lote.rds")

IPTU <- read.csv("dados/IPTU/IPTU_2024.csv", sep=";", encoding = "latin1") |>  
  as_tibble() |>  
  select(sql = "NUMERO.DO.CONTRIBUINTE", 
         condominio = "NUMERO.DO.CONDOMINIO",
         area_terreno = "AREA.DO.TERRENO",
         area_construida = "AREA.CONSTRUIDA",
         area_ocupada = "AREA.OCUPADA",
         pavimentos = "QUANTIDADE.DE.PAVIMENTOS",
         ano_construcao = "ANO.DA.CONSTRUCAO.CORRIGIDO",
         tipo = "TIPO.DE.PADRAO.DA.CONSTRUCAO") |>  
  # Separação do número de contribuinte (SQL) em setor quadra e lote
  mutate(setor =  str_sub(sql, 1, 3),
         quadra = str_sub(sql, 4, 6),
         # Quando o lote é um condomínio, haverá vários SQLs no mesmo lote. CD = Condomínio
         lote = str_sub(sql, 7, 10) |>  
           ifelse(condominio == "00-0", yes = _, paste("CD", str_sub(condominio, 1, 2), sep = ""))) |>
  filter(str_detect(tipo, "Residencial")) |> 
  # Agregar por SQL
  group_by(setor, quadra, lote) |>  
  summarize(unidades = n(),
            area_terreno = median(area_terreno), 
            area_construida = sum(area_construida), 
            area_ocupada = median(area_ocupada),
            pavimentos = median(pavimentos),
            ano_construcao = median(ano_construcao)) |>  
  ungroup() |>  
  mutate(densidade_construtiva = area_construida / area_terreno,
         densidade_habitacional = unidades / area_terreno,
         verticalizacao = area_construida / area_ocupada)

lotes <- list.files(path="dados/lotes/unzip", full.names = FALSE) |>  
  (\(.) paste("dados/lotes/unzip/", ., "/", ., ".shp", sep = ""))() |>  
  lapply(read_sf) |>  
  bind_rows() |>  
  st_set_crs("epsg:31983") |> 
  mutate(lo_lote = ifelse(lo_lote == "0000", paste("CD", lo_condomi, sep = ""), lo_lote),
         lo_lote = ifelse(lo_tp_lote != "F", paste(lo_tp_lote, lo_lote, sep = ""), lo_lote)) |>  
  select(setor = lo_setor, quadra = lo_quadra, lote = lo_lote, tipo_lote = lo_tp_lote)

IPTU.lote <- lotes |>  
  right_join(IPTU, by = join_by(setor, quadra, lote)) |>  
  ungroup()


censo2022 <- read_sf("dados/censo/2022/SP_Malha_Preliminar_2022.shp") |> 
  filter(CD_MUN == "3550308") |> 
  st_transform("epsg:31983")  |>
  select(id_setor = CD_SETOR, moradores = v0001, domicilios = v0002)


distrito.IPTU <- distrito |> 
  st_join(IPTU.lote |> 
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

destaque <- c("MOEMA", "JARDIM PAULISTA", "PINHEIROS", "VILA SONIA", "CAPAO REDONDO")


df |> 
  drop_na() |> 
  filter(row_number(abs(espectro_irregularidade - .5)) <= 40) |>
  mutate(across(c(verticalizacao, densidade_construtiva, densidade_habitacional, densidade, densidade_residencial), ~ row_number(-.x)),
         distancia = densidade_residencial - verticalizacao,
         selected = ifelse(ds_nome %in% destaque, ds_nome, NA),
         alpha = ifelse(ds_nome %in% destaque, 1, 0)) |> 
  pivot_longer(c(verticalizacao, densidade_construtiva, densidade_habitacional, densidade_residencial)) |> 
  mutate(ds_nome = str_to_title(ds_nome),
         label_direita = ifelse(name == "verticalizacao", paste("(", str_pad(value, width = 2, side = "left", pad = "0"), ") ", ds_nome, sep = ""), NA),
         label_esquerda = ifelse(name == "densidade_residencial", paste(ds_nome, " (", str_pad(value, width = 2, side = "left", pad = "0"), ")", sep = ""), NA)) |> 
  ggplot(aes(x = name, y = value, group = ds_nome)) +
  geom_bump(colour = "gray90", linewidth = 0.8, smooth = 6) +
  geom_bump(aes(colour = selected, alpha = alpha), linewidth = 0.8, smooth = 6) +
  geom_point(colour = "white", size = 4) +
  geom_point(colour = "gray90", size = 2) +
  geom_point(aes(colour = selected, alpha = alpha), size = 2) +
  geom_text(aes(label = label_direita), x = 4.1, hjust = "left", color = "gray50") +
  geom_text(aes(label = label_esquerda), x = .9, hjust = "right", color = "gray50") +
  scale_colour_paletteer_d("fishualize::Balistapus_undulatus") +
  scale_y_reverse("", breaks = c()) +  
  scale_x_discrete("", limits = c("densidade_residencial", 
                                  "densidade_habitacional",
                                  "densidade_construtiva",
                                  "verticalizacao"),
                   labels = c("Densidade\n populacional", "Densidade\n habitacional", "Densidade\n construtiva", "Verticalização"),
                   expand = expansion(add = c(1.2,1.2))) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = 14, hjust = .5),
        plot.subtitle = element_text(size = 10, hjust = .5),
        plot.caption = element_text(size = 8))

ggsave("tex/figuras/bairros.pdf", width = 9, height = 7)


df |> 
  drop_na() |> 
  # filter(row_number(abs(espectro_irregularidade - .5)) <= 40) |>
  mutate(across(c(verticalizacao, densidade_construtiva, densidade_habitacional, densidade, densidade_residencial), ~ row_number(-.x)),
         distancia = densidade_residencial - verticalizacao) |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = distancia)) + 
  scale_fill_gradient2("Diferença de\n posições", midpoint = 0, low = "darkblue", high = "darkred", limits = c(-100, 100)) +
  theme_void()

ggsave("tex/figuras/bairros-mapa.pdf", width = 8, height = 7)



df |> 
  drop_na() |> 
  # filter(row_number(abs(espectro_irregularidade - .5)) <= 40) |>
  mutate(across(c(verticalizacao, densidade_construtiva, densidade_habitacional, densidade, densidade_residencial), ~ row_number(-.x)),
         distancia = densidade_residencial - densidade_habitacional) |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = distancia)) + 
  scale_fill_gradient2("Diferença de\n posições", midpoint = 0, low = "darkblue", high = "darkred") +
  theme_void()


ggsave("tex/figuras/bairros-mapa-cotaparte.pdf", width = 8, height = 7)


