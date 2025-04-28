post_overline = function(
                      rnet,
                      segment_length     = 10) {

  message("Downloading active‑travel network…")
  osm_net = osmactive::get_travel_network("scotland")
  osm_net = get_cycling_network(osm_net)  

  rnet = rnet |>
    sf::st_transform(27700)
  osm_net = osm_net |>
    sf::st_transform(sf::st_crs(rnet))

  bounds = rnet |>
    sf::st_union() |>
    sf::st_convex_hull() |>
    sf::st_transform(sf::st_crs(osm_net))

  base_network = osm_net[bounds, ]

  base_net = sf::st_transform(base_network, sf::st_crs(rnet))

  cols_to_keep = c("osm_id", "geometry")
  base_short   = base_net[cols_to_keep]

  overlapping_joined = stplanr::rnet_join(
    base_short,
    rnet,
    segment_length = segment_length
  )

  overlapping_aggregated = overlapping_joined |>
    sf::st_drop_geometry() |>
    dplyr::mutate(across(matches("bicycle"), function(x) x * length_y)) |>
    dplyr::group_by(osm_id) |>
    dplyr::summarise(across(matches("bicycle"), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

  # 4. Length‑normalise -------------------------------------------------------------
  base_net$length_x = sf::st_length(base_net) |> as.numeric()

  rnet_joined = dplyr::left_join(
    base_net |> dplyr::select(osm_id, length_x, geometry),
    overlapping_aggregated,
    by = "osm_id"
  )

  rnet_final = rnet_joined |>
    dplyr::mutate(across(matches("bicycle"), \(x) x / length_x))

  rnet_final
}