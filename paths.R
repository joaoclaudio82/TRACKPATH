# 1. PACOTES

libs <- c(
  "tidyverse", "osmdata",
  "dodgr", "sf", "maptiles",
  "tidyterra"
)

installed_libs <- libs %in% rownames(
  installed.packages()
)

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs],
    dependencies = T
  )
}

invisible(
  lapply(
    libs, library,
    character.only = T
  )
)

# 2. BOUNDING BOX
# 20.417487,44.800668,20.484438,44.832196

# Define as coordenadas da bounding box para Fortaleza
xmin <- -38.528572
ymin <- -3.747306
xmax <- -38.500677
ymax <- -3.723166


fortaleza_bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax) |>
  sf::st_bbox(crs = 4326) |>
  sf::st_as_sfc(crs = 4326)

# Obtém a camada de rua da área definida
fortaleza_streets <- maptiles::get_tiles(
  fortaleza_bbox,
  provider = "CartoDB.Positron",
  zoom = 14,
  crop = T,
  forceDownload = T
)

# 
# belgrade_bbox <- c(
#   xmin = xmin, ymin = ymin,
#   xmax = xmax, ymax = ymax
# ) |>
#   sf::st_bbox(crs = 4326) |>
#   sf::st_as_sfc(crs = 4326)

# 3. GET STREET LAYER

# belgrade_streets <- maptiles::get_tiles(
#   belgrade_bbox,
#   provider = "CartoDB.Positron",
#   zoom = 14,
#   crop = T,
#   # project = F,
#   forceDownload = T
# )

ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = fortaleza_streets
  )

# 4. GET FUEL STATIONS

fortaleza_amenities <- osmdata::opq(
  bbox = fortaleza_bbox,
  timeout = 180,
  memsize = 104857600
) |>
  osmdata::add_osm_feature(
    key = "amenity",
    value = "fuel"
  ) |>
  osmdata::osmdata_sf()
fortaleza_fuel_pts <- fortaleza_amenities[c("osm_points")]
fortaleza_fuel <- do.call(rbind, fortaleza_fuel_pts) |>
  dplyr::select("osm_id", "geometry")
# Visualiza postos de combustível no mapa
ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = fortaleza_streets
  ) +
  geom_sf(
    data = fortaleza_fuel,
    color = "blue",
    inherit.aes = F
  )


# 5. GET OSM ROADS

# Extrai dados de estradas da OpenStreetMap
fortaleza_roads <- osmdata::opq(
  bbox = fortaleza_bbox,
  timeout = 180,
  memsize = 104857600
) |>
  osmdata::add_osm_feature(
    key = "highway"
  ) |>
  osmdata::osmdata_sf()
fortaleza_hways <- fortaleza_roads$osm_lines

ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = fortaleza_streets
  ) +
  geom_sf(
    data = fortaleza_fuel,
    color = "blue",
    inherit.aes = F
  ) +
  geom_sf(
    data = fortaleza_hways,
    color = "black",
    size = .15,
    alpha = .5,
    inherit.aes = F
  ) +
  theme_void()

# 6. DECOMPOSE ROADS INTO DISTINCT EDGES

g <- dodgr::weight_streetnet(
  fortaleza_hways,
  wt_profile = "motorcar",
  type_col = "highway"
)

head(g)

# 7. CALCULATE PATHS

v <- dodgr::dodgr_vertices(g)

xy <- v[sample(
  nrow(v),
  size = 1
),]

to <- sf::st_coordinates(fortaleza_fuel)

paths <- dodgr::dodgr_paths(
  graph = g,
  from = xy,
  to = to
)

# 8. MATCH PATHS AND TURN THEM TO SF

paths_sf <- lapply(
  paths, function(x) {
    lapply(
      x, function(y) {
        path_xy <- v[match(y, v$id), ]
        # Certifica-se de que 'path_xy' contém apenas as colunas 'x' e 'y' e que ambas são numéricas
        if (!is.null(path_xy) && all(c("x", "y") %in% names(path_xy)) &&
            is.numeric(path_xy$x) && is.numeric(path_xy$y)) {
          # Converte 'path_xy' em uma matriz e cria um linestring
          linestring <- sf::st_linestring(as.matrix(path_xy[, c("x", "y")]))
          # Converte linestring para sfc com CRS definido
          sf::st_sfc(linestring, crs = 4326)
        } else {
          NULL # Retorna NULL se 'path_xy' não estiver no formato esperado
        }
      }
    )
  }
)


paths_sf <- lapply(
  paths_sf, function(x) {
    do.call(
      rbind, x
    )
  }
)

paths_sf <- do.call(rbind, paths_sf)

paths_sf <- sf::st_sf(
  from = rep(
    names(paths),
    each = nrow(xy)
  ),
  to = rep(
    names(paths),
    times = nrow(xy)
  ),
  geometry = paths_sf[, 1],
  crs = 4326
)

ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = fortaleza_streets
  ) +
  geom_sf(
    data = fortaleza_fuel,
    color = "blue",
    inherit.aes = F
  ) +
  geom_point(
    data = xy,
    aes(x = x, y = y),
    color = "red",
    inherit.aes = F
  ) +
  geom_sf(
    data = paths_sf,
    color = "black",
    alpha = .65,
    size = .25,
    inherit.aes = F
  ) +
  theme_void()



# 9. WHAT IS THE SHORTEST?

# compute distances
route_distances <- dodgr::dodgr_dists(
  graph = g,
  from = xy,
  to = to,
  shortest = T
)

head(route_distances)

route_distances_df <- route_distances |>
  as.matrix() |>
  as.data.frame() |>
  t()

head(route_distances_df)

shortest_route_distance_df <- cbind(
  edge_id = rownames(
    route_distances_df
  ),
  route_distances_df
) |>
  as.data.frame() |>
  dplyr::rename(
    distance = `10652416518`
  ) |>
  dplyr::mutate(
    distance = as.numeric(distance)
  ) |>
  dplyr::arrange(distance) |>
  dplyr::distinct(
    edge_id, distance,
    .keep_all = T
  ) |>
  dplyr::slice_head(n = 1)

head(shortest_route_distance_df)

# get all paths

df_graph_edges <- paths |>
  unlist() |>
  as.matrix() |>
  as.data.frame()

head(df_graph_edges)

df_graph_edges <- cbind(
  id = rownames(
    df_graph_edges
  ), df_graph_edges
)

head(df_graph_edges)

names(df_graph_edges)[2] <- "edge_id"

# find shortest path among all paths

shortest_line <- df_graph_edges |>
  dplyr::filter(
    stringr::str_detect(
      id,
      shortest_route_distance_df$edge_id
    )
  )

head(shortest_line)

# fetch shortest line from street network

gsf <- dodgr::dodgr_to_sf(
  g
)

shortest_path_sf <- gsf |>
  dplyr::filter(
    to_id %in% unique(
      shortest_line$edge_id
    )
  ) |>
  sf::st_intersection(paths_sf)

ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = fortaleza_streets
  ) +
  geom_sf(
    data = fortaleza_fuel,
    color = "blue",
    inherit.aes = F
  ) +
  geom_point(
    data = xy,
    aes(x = x, y = y),
    color = "red",
    size = 1.25,
    inherit.aes = F
  ) +
  geom_sf(
    data = paths_sf,
    color = "black",
    alpha = .5,
    size = .25,
    inherit.aes = F
  ) +
  geom_sf(
    data = shortest_path_sf,
    color = "red",
    size = .5,
    inherit.aes = F
  ) +
  theme_void()

# Finalmente, visualiza o caminho mais curto no mapa
# Inicia a construção de um gráfico usando ggplot2.
ggplot() +
  # Adiciona a camada de rua obtida de 'belgrade_streets' como fundo do mapa.
  tidyterra::geom_spatraster_rgb(data = fortaleza_streets) +
  # Adiciona os pontos de combustível ao mapa com cor azul.
  geom_sf(data = fortaleza_fuel, color = "blue", inherit.aes = F) +
  # Adiciona o ponto de origem ao mapa com cor vermelha.
  geom_point(data = xy, aes(x = x, y = y), color = "red", size = 1.25, inherit.aes = F) +
  # Adiciona todos os caminhos calculados ao mapa com cor preta.
  geom_sf(data = paths_sf, color = "black", alpha = .5, size = .25, inherit.aes = F) +
  # Destaca o caminho mais curto no mapa com cor vermelha.
  geom_sf(data = shortest_path_sf, color = "red", size = .5, inherit.aes = F) +
  # Aplica um tema sem elementos adicionais para focar no mapa.
  theme_void()

