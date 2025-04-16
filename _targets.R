# Instructions
# 1) library(targets)
# 2) Optional: tar_visnetwork(TRUE) - See the current status of the targets
# 3) Optional: update_github_packages = TRUE, see line 12
# 4) Optional: uncomment controller = crew_controller_local(workers = 4) for multicore running, line 23
# 5) Optional: tar_watch(seconds = 60, targets_only = TRUE, supervise = FALSE, poll_connection = FALSE)
# 6) tar_make()

# Options
# Do you want to reinstall github packages, set to TRUE for first run
update_github_packages = TRUE

# Run the install script
if (!"corenet" %in% installed.packages()) {
  source("code/install.R")
}

# Load minimum of libraries (Should use package::function in most cases)
library(targets) # Needed to make targets work
library(magrittr) # Light load of |>
library(sf) # Needed for sf support
set.seed(2023)
httr::set_config(httr::timeout(seconds = 60000))
tar_source()
pkgs = get_pkgs()


tar_option_set(
  controller = crew::crew_controller_local(workers = 1),
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  packages = pkgs,
  format = "rds" # default storage format
)

# Targets -----------------------------------------------------------------
if (!file.exists("outputdata")) {
  stop("You don't have the outputdata folder")
}

list(
  # Detect when parameter file has changed:
  tar_target(name = param_file, command = "parameters.json", format = "file"),
  # Check Renviron exists, create output directory, load params:
  tar_target(parameters, {
    renviron_exists = file.exists(".Renviron")
    if (!renviron_exists) {
      warning("No .Renviron file, routing may not work")
    }
    jsonlite::read_json(param_file, simplifyVector = TRUE)
  }),
  tar_target(
    dir_local,
    {
      dir_local = paste0("outputdata/", parameters$date_routing)
      dir.create(dir_local, showWarnings = FALSE)
      dir_local
    }
  ),
  tar_target(
    region_folder,
    {
      region_name_lower = snakecase::to_snake_case(parameters$region)
      folder_name = file.path(dir_local, region_name_lower)
      dir.create(file.path(folder_name), recursive = TRUE, showWarnings = FALSE)
      folder_name
    }
  ),
  tar_target(aadt_file, command = "data-raw/AADT_factors.csv", format = "file"),
  tar_target(aadt_parameters, {
    readr::read_csv(aadt_file)
  }),
  # See build.R:
  tar_target(boundaries_file, "la_regions_scotland_bfe_simplified_2023.geojson", format = "file"),
  tar_target(
    local_authorities,
    sf::read_sf(boundaries_file)
  ),
  tar_target(
    region_names,
    unique(local_authorities$Region)
  ),

  # Case study area:
  tar_target(
    local_authorities_region,
    {
      local_authorities_region = local_authorities |>
        filter(Region == parameters$region)
      sf::write_sf(local_authorities_region, file.path(region_folder, "local_authorities_region.geojson"), delete_dsn = TRUE)
      local_authorities_region
    }
  ),
  tar_target(
    region_boundary,
    local_authorities_region
    |> sf::st_union()
  ),
  tar_target(
    region_boundary_buffered,
    region_boundary |>
      stplanr::geo_buffer(dist = parameters$region_buffer_distance)
  ),
  tar_target(zones_boundary_buffered, {
    z = readRDS("inputdata/DataZones.Rds") # 6976 zones
    # z_centroids = sf::st_point_on_surface(z)
    # z_centroids_within = z_centroids[region_boundary_buffered, ]
    # z[z[[1]] %in% z_centroids_within[[1]], ]
    z
  }),
  tar_target(zones, {
    z = zones_boundary_buffered
    z
  }),
  tar_target(od_national, {
    if (parameters$open_data_build) {
      od_raw = read_csv("data-raw/od_data_dz_synthetic.csv")
      # % cycling in national od data
      # sum(od_raw$bicycle) / sum(od_raw$all) # 0.15
    } else {
      desire_lines_raw = read_TEAMS("secure_data/commute/commute_dl_sub30km.Rds")
      od_raw = as_tibble(sf::st_drop_geometry(desire_lines_raw))
    }
    od_subset = od_raw |>
      filter(geo_code1 %in% zones$DataZone) |>
      filter(geo_code2 %in% zones$DataZone) |>
      filter(dist_euclidean < 20000) |>
      filter(dist_euclidean > 1000) |>
      filter(all >= parameters$min_flow) |>
      mutate_od_commute()
    od_subset
  }),
  tar_target(subpoints_origins, {
    # source("data-raw/get_wpz.R")
    # sf::read_sf("data-raw/oas.geojson")
    if (parameters$open_data_build) {
      # create a sample of randomly located points in each zone:
      spo = st_sample(zones, size = nrow(zones) * 20, by_polygon = TRUE)
    } else {
      spo = readRDS("inputdata/oas.Rds")
    }
    spo[region_boundary, ]
  }),
  tar_target(subpoints_destinations, {
    # source("data-raw/get_wpz.R")
    # sf::read_sf("data-raw/workplaces_simple_edinburgh.geojson")
    # readRDS("inputdata/workplaces_simple.Rds") #Not enough points when using DataZones
    message("Getting destinations")
    spd = NULL
    if (parameters$open_data_build) {
      # message("Getting destinations for ", nrow(zones), " zones")
      # spd = st_sample(zones, size = nrow(zones) * 20, by_polygon = TRUE)
      spd = subpoints_origins
    } else {
      spd = read_TEAMS("secure_data/OS/os_poi.Rds")
      spd = spd[spd$workplace, ]
    }
    spd[region_boundary_buffered, ]
  }),
  tar_target(od_commute_jittered, {
    # od_jittered = od_national # for no jittering
    # Install the Rust crate and the associated R package:
    # system("cargo install --git https://github.com/dabreegster/odjitter")
    z = zones
    # z = z[subpoints_destinations, ]
    od_national |>
      filter(geo_code1 %in% z$DataZone) |>
      filter(geo_code2 %in% z$DataZone)

    # odj = odjitter::jitter(
    #   od = od,
    #   zones = z,
    #   subpoints_origins = subpoints_origins,
    #   subpoints_destinations = subpoints_destinations,
    #   disaggregation_threshold = 30,
    #   deduplicate_pairs = FALSE
    # )
    # odj$dist_euclidean_jittered = as.numeric(sf::st_length(odj))
    # odj
  }),
  tar_target(od_commute_subset, {
    odcs = od_commute_jittered |>
      filter(dist_euclidean < 16000) |>
      filter(dist_euclidean > 1000) |>
      slice_max(n = parameters$max_to_route, order_by = all, with_ties = FALSE)
      odcs = od::od_to_sf(odcs, zones)
      odcs
  }),


  # School OD ---------------------------------------------------------------

  tar_target(od_school, {
    # Get School OD
    if (parameters$open_data_build) {
      schools_dl = sf::read_sf("data-raw/school_desire_lines_open.geojson")
    } else {
      schools_dl = read_TEAMS("secure_data/schools/school_dl_sub30km.Rds")
    }
    # schools_dl = schools_dl[region_boundary_buffered, op = sf::st_within]
    schools_dl$dist_euclidean_jittered = round(as.numeric(sf::st_length(schools_dl)))
    schools_dl = schools_dl |>
      filter(dist_euclidean_jittered < 10000) |>
      filter(dist_euclidean_jittered > 1000) |>
      slice_max(order_by = all, n = parameters$max_to_route, with_ties = FALSE) |>
      mutate_od_school()
    schools_dl
  }),

  # # School routing ----------------------------------------------------------

  # tar_target(rs_school_fastest, {
  #   rs = get_routes(
  #     od = od_school |> slice_max(n = parameters$max_to_route, order_by = all, with_ties = FALSE),
  #     plans = "fastest",
  #     purpose = "school",
  #     folder = region_folder,
  #     date = parameters$date_routing,
  #     segments = "both"
  #   )
  #   rs
  # }),
  # tar_target(done_school_fastest, {
  #   length(rs_school_fastest) # Hack for scheduling
  # }),
  # tar_target(rs_school_quietest, {
  #   length(done_school_fastest)
  #   rs = get_routes(
  #     od = od_school |> slice_max(n = parameters$max_to_route, order_by = all, with_ties = FALSE),
  #     plans = "quietest",
  #     purpose = "school",
  #     folder = region_folder,
  #     date = parameters$date_routing,
  #     segments = "both"
  #   )
  #   rs
  # }),
  # tar_target(done_school_quietest, {
  #   length(rs_school_quietest) # Hack for scheduling
  # }),

  # # Balanced:
  # tar_target(rs_school_balanced, {
  #   length(done_school_quietest)
  #   rs = get_routes(
  #     od = od_school |> slice_max(n = parameters$max_to_route, order_by = all, with_ties = FALSE),
  #     plans = "balanced",
  #     purpose = "school",
  #     folder = region_folder,
  #     date = parameters$date_routing,
  #     segments = "both"
  #   )
  #   rs
  # }),
  # tar_target(done_school_balanced, {
  #   length(rs_school_balanced) # Hack for scheduling
  # }),
  # tar_target(rs_school_ebike, {
  #   length(done_school_quietest)
  #   rs = get_routes(
  #     od = od_school |> slice_max(n = parameters$max_to_route, order_by = all, with_ties = FALSE),
  #     plans = "ebike",
  #     purpose = "school",
  #     folder = region_folder,
  #     date = parameters$date_routing,
  #     segments = "both"
  #   )
  #   rs
  # }),
  # tar_target(done_school_ebike, {
  #   length(rs_school_ebike) # Hack for scheduling
  # }),

  # # Commute routing ---------------------------------------------------------

  # tar_target(rs_commute_fastest, {
  #   length(done_school_ebike) # Do school routing first
  #   rs = get_routes(
  #     od = od_commute_subset,
  #     plans = "fastest",
  #     purpose = "commute",
  #     folder = region_folder,
  #     date = parameters$date_routing,
  #     segments = "both"
  #   )
  #   rs
  # }),
  # tar_target(done_commute_fastest, {
  #   length(rs_commute_fastest) # Hack for scheduling
  # }),
  # tar_target(rs_commute_balanced, {
  #   length(done_commute_fastest)
  #   rs = get_routes(
  #     od = od_commute_subset,
  #     plans = "balanced",
  #     purpose = "commute",
  #     folder = region_folder,
  #     date = parameters$date_routing,
  #     segments = "both"
  #   )
  #   rs
  # }),
  # tar_target(done_commute_balanced, {
  #   length(rs_commute_balanced) # Hack for scheduling
  # }),
  # tar_target(rs_commute_quietest, {
  #   length(done_commute_fastest)
  #   rs = get_routes(
  #     od = od_commute_subset,
  #     plans = "quietest",
  #     purpose = "commute",
  #     folder = region_folder,
  #     date = parameters$date_routing,
  #     segments = "both"
  #   )
  #   rs
  # }),
  # tar_target(done_commute_quietest, {
  #   length(rs_commute_quietest) # Hack for scheduling
  # }),
  # tar_target(rs_commute_ebike, {
  #   length(done_commute_quietest)
  #   rs = get_routes(
  #     od = od_commute_subset,
  #     plans = "ebike",
  #     purpose = "commute",
  #     folder = region_folder,
  #     date = parameters$date_routing,
  #     segments = "both"
  #   )
  #   rs
  # }),
  # tar_target(done_commute_ebike, {
  #   length(rs_commute_ebike) # Hack for scheduling
  # }),

  # # Commute routing post-processing -----------------------------------------

  # tar_target(r_commute_fastest, {
  #   rs_commute_fastest[[1]]$routes
  # }),
  # tar_target(r_commute_quietest, {
  #   rs_commute_quietest[[1]]$routes
  # }),
  # tar_target(r_commute_ebike, {
  #   rs_commute_ebike[[1]]$routes
  # }),
  # tar_target(r_commute_balanced, {
  #   rs_commute_balanced[[1]]$routes
  # }),
  # tar_target(rnet_gq_commute_fastest, {
  #   segments2rnet(rs_commute_fastest[[1]]$segments)
  # }),
  # tar_target(rnet_gq_commute_quietest, {
  #   segments2rnet(rs_commute_quietest[[1]]$segments)
  # }),
  # tar_target(rnet_gq_commute_ebike, {
  #   segments2rnet(rs_commute_ebike[[1]]$segments)
  # }),
  # tar_target(rnet_gq_commute_balanced, {
  #   segments2rnet(rs_commute_balanced[[1]]$segments)
  # }),

  # # School routing post-processing -----------------------------------------

  # tar_target(r_school_fastest, {
  #   rs_school_fastest[[1]]$routes
  # }),
  # tar_target(r_school_quietest, {
  #   rs_school_quietest[[1]]$routes
  # }),
  # tar_target(r_school_ebike, {
  #   rs_school_ebike[[1]]$routes
  # }),
  # tar_target(r_school_balanced, {
  #   rs_school_balanced[[1]]$routes
  # }),
  # tar_target(rnet_gq_school_fastest, {
  #   segments2rnet(rs_school_fastest[[1]]$segments)
  # }),
  # tar_target(rnet_gq_school_quietest, {
  #   segments2rnet(rs_school_quietest[[1]]$segments)
  # }),
  # tar_target(rnet_gq_school_ebike, {
  #   segments2rnet(rs_school_ebike[[1]]$segments)
  # }),
  # tar_target(rnet_gq_school_balanced, {
  #   segments2rnet(rs_school_balanced[[1]]$segments)
  # }),

  # # Commute Uptake ----------------------------------------------------------

  # tar_target(uptake_commute_fastest, {
  #   r_commute_fastest |>
  #     aadt_adjust(purpose = "commute", aadt_parameters = aadt_parameters) |>
  #     get_uptake_scenarios()
  # }),
  # tar_target(uptake_commute_quietest, {
  #   r_commute_quietest |>
  #     aadt_adjust(purpose = "commute", aadt_parameters = aadt_parameters) |>
  #     get_uptake_scenarios()
  # }),
  # tar_target(uptake_commute_ebike, {
  #   r_commute_ebike |>
  #     aadt_adjust(purpose = "commute", aadt_parameters = aadt_parameters) |>
  #     get_uptake_scenarios()
  # }),
  # tar_target(uptake_commute_balanced, {
  #   r_commute_balanced |>
  #     aadt_adjust(purpose = "commute", aadt_parameters = aadt_parameters) |>
  #     get_uptake_scenarios()
  # }),

  # # School Uptake ----------------------------------------------------------

  # tar_target(uptake_school_fastest, {
  #   routes = r_school_fastest |>
  #     aadt_adjust(purpose = "school", aadt_parameters = aadt_parameters) |>
  #     get_uptake_scenarios(purpose = "school")
  #   routes
  # }),
  # tar_target(uptake_school_quietest, {
  #   routes = r_school_quietest |>
  #     aadt_adjust(purpose = "school", aadt_parameters = aadt_parameters) |>
  #     get_uptake_scenarios(purpose = "school")
  #   routes
  # }),
  # tar_target(uptake_school_ebike, {
  #   routes = r_school_ebike |>
  #     aadt_adjust(purpose = "school", aadt_parameters = aadt_parameters) |>
  #     get_uptake_scenarios(purpose = "school")
  #   routes
  # }),
  # tar_target(uptake_school_balanced, {
  #   routes = r_school_balanced |>
  #     aadt_adjust(purpose = "school", aadt_parameters = aadt_parameters) |>
  #     get_uptake_scenarios(purpose = "school")
  #   routes
  # }),


  # # Commute RNets -----------------------------------------------------------

  # tar_target(rnet_commute_fastest, {
  #   stplanr::overline2(uptake_commute_fastest, c("bicycle", "bicycle_go_dutch", "bicycle_ebike"))
  # }),
  # tar_target(rnet_commute_quietest, {
  #   stplanr::overline2(uptake_commute_quietest, c("bicycle", "bicycle_go_dutch", "bicycle_ebike"))
  # }),
  # tar_target(rnet_commute_ebike, {
  #   stplanr::overline2(uptake_commute_ebike, c("bicycle", "bicycle_go_dutch", "bicycle_ebike"))
  # }),
  # tar_target(rnet_commute_balanced, {
  #   stplanr::overline2(uptake_commute_balanced, c("bicycle", "bicycle_go_dutch", "bicycle_ebike"))
  # }),


  # # Primary School RNets -----------------------------------------------------------

  # tar_target(rnet_primary_fastest, {
  #   rnet = uptake_school_fastest
  #   rnet = rnet[rnet$schooltype == "primary", ]
  #   rnet = stplanr::overline2(rnet, c("bicycle", "bicycle_go_dutch", "bicycle_ebike"))
  #   saveRDS(rnet, paste0(region_folder, "/rnet_primary_school_fastest.Rds"))
  #   rnet
  # }),
  # tar_target(rnet_primary_quietest, {
  #   rnet = uptake_school_quietest
  #   rnet = rnet[rnet$schooltype == "primary", ]
  #   rnet = stplanr::overline2(rnet, c("bicycle", "bicycle_go_dutch", "bicycle_ebike"))
  #   saveRDS(rnet, paste0(region_folder, "/rnet_primary_school_quietest.Rds"))
  #   rnet
  # }),
  # tar_target(rnet_primary_ebike, {
  #   rnet = uptake_school_ebike
  #   rnet = rnet[rnet$schooltype == "primary", ]
  #   rnet = stplanr::overline2(rnet, c("bicycle", "bicycle_go_dutch", "bicycle_ebike"))
  #   saveRDS(rnet, paste0(region_folder, "/rnet_primary_school_ebike.Rds"))
  #   rnet
  # }),
  # tar_target(rnet_primary_balanced, {
  #   rnet = uptake_school_balanced
  #   rnet = rnet[rnet$schooltype == "primary", ]
  #   rnet = stplanr::overline2(rnet, c("bicycle", "bicycle_go_dutch", "bicycle_ebike"))
  #   saveRDS(rnet, paste0(region_folder, "/rnet_primary_school_balanced.Rds"))
  #   rnet
  # }),

  # # Secondary School RNets -----------------------------------------------------------

  # tar_target(rnet_secondary_fastest, {
  #   rnet = uptake_school_fastest
  #   rnet = rnet[rnet$schooltype == "secondary", ]
  #   rnet = stplanr::overline2(rnet, c("bicycle", "bicycle_go_dutch", "bicycle_ebike"))
  #   saveRDS(rnet, paste0(region_folder, "/rnet_primary_school_fastest.Rds"))
  #   rnet
  # }),
  # tar_target(rnet_secondary_quietest, {
  #   rnet = uptake_school_quietest
  #   rnet = rnet[rnet$schooltype == "secondary", ]
  #   rnet = stplanr::overline2(rnet, c("bicycle", "bicycle_go_dutch", "bicycle_ebike"))
  #   saveRDS(rnet, paste0(region_folder, "/rnet_primary_school_quietest.Rds"))
  #   rnet
  # }),
  # tar_target(rnet_secondary_ebike, {
  #   rnet = uptake_school_ebike
  #   rnet = rnet[rnet$schooltype == "secondary", ]
  #   rnet = stplanr::overline2(rnet, c("bicycle", "bicycle_go_dutch", "bicycle_ebike"))
  #   saveRDS(rnet, paste0(region_folder, "/rnet_primary_school_ebike.Rds"))
  #   rnet
  # }),
  # tar_target(rnet_secondary_balanced, {
  #   rnet = uptake_school_balanced
  #   rnet = rnet[rnet$schooltype == "secondary", ]
  #   rnet = stplanr::overline2(rnet, c("bicycle", "bicycle_go_dutch", "bicycle_ebike"))
  #   saveRDS(rnet, paste0(region_folder, "/rnet_primary_school_balanced.Rds"))
  #   rnet
  # }),

  # # Commute Zone stats ---------------------------------------------------------

  # tar_target(commute_stats_baseline, {
  #   stats = sf::st_drop_geometry(od_commute_subset)
  #   stats = aadt_adjust(stats, purpose = "commute", aadt_parameters = aadt_parameters)
  #   stats_from = dplyr::group_by(stats, geo_code1) |>
  #     dplyr::summarise(
  #       all = sum(all, na.rm = TRUE),
  #       bicycle = sum(bicycle, na.rm = TRUE),
  #       car = sum(car, na.rm = TRUE),
  #       foot = sum(foot, na.rm = TRUE),
  #       public_transport = sum(public_transport, na.rm = TRUE),
  #       taxi = sum(taxi, na.rm = TRUE)
  #     )
  #   stats_to = dplyr::group_by(stats, geo_code2) |>
  #     dplyr::summarise(
  #       all = sum(all, na.rm = TRUE),
  #       bicycle = sum(bicycle, na.rm = TRUE),
  #       car = sum(car, na.rm = TRUE),
  #       foot = sum(foot, na.rm = TRUE),
  #       public_transport = sum(public_transport, na.rm = TRUE),
  #       taxi = sum(taxi, na.rm = TRUE)
  #     )

  #   names(stats_from)[1] = "DataZone"
  #   names(stats_to)[1] = "DataZone"

  #   names(stats_from)[2:ncol(stats_from)] = paste0("comm_orig_", names(stats_from)[2:ncol(stats_from)])
  #   names(stats_to)[2:ncol(stats_to)] = paste0("comm_dest_", names(stats_to)[2:ncol(stats_to)])
  #   stats = dplyr::full_join(stats_from, stats_to, by = "DataZone")
  #   stats
  # }),
  # tar_target(commute_stats_fastest, {
  #   make_commute_stats(uptake_commute_fastest, "fastest")
  # }),
  # tar_target(commute_stats_quietest, {
  #   make_commute_stats(uptake_commute_quietest, "quietest")
  # }),
  # tar_target(commute_stats_ebike, {
  #   make_commute_stats(uptake_commute_ebike, "ebike")
  # }),
  # tar_target(commute_stats_balanced, {
  #   make_commute_stats(uptake_commute_balanced, "balanced")
  # }),


  # # School Zone stats ---------------------------------------------------------

  # tar_target(school_stats_baseline, {
  #   stats = sf::st_drop_geometry(od_school)
  #   stats = aadt_adjust(stats, purpose = "school", aadt_parameters = aadt_parameters)
  #   stats = dplyr::group_by(stats, SeedCode, schooltype) |>
  #     dplyr::summarise(
  #       all = sum(all, na.rm = TRUE),
  #       bicycle = sum(bicycle, na.rm = TRUE),
  #       car = sum(car, na.rm = TRUE),
  #       foot = sum(foot, na.rm = TRUE),
  #       public_transport = sum(public_transport, na.rm = TRUE),
  #       other = sum(other, na.rm = TRUE)
  #     )

  #   stats$schooltype = paste0("schl_", stats$schooltype, "_dest")


  #   stats = tidyr::pivot_wider(stats,
  #     id_cols = c("SeedCode"),
  #     names_from = c("schooltype"),
  #     names_glue = "{schooltype}_{.value}",
  #     values_from = names(stats)[3:ncol(stats)]
  #   )
  #   stats
  # }),
  # tar_target(school_stats_from_baseline, {
  #   stats = sf::st_drop_geometry(od_school)
  #   stats = aadt_adjust(stats, purpose = "school", aadt_parameters = aadt_parameters)
  #   stats = dplyr::group_by(stats, DataZone, schooltype) |>
  #     dplyr::summarise(
  #       all = sum(all, na.rm = TRUE),
  #       bicycle = sum(bicycle, na.rm = TRUE),
  #       car = sum(car, na.rm = TRUE),
  #       foot = sum(foot, na.rm = TRUE),
  #       public_transport = sum(public_transport, na.rm = TRUE),
  #       other = sum(other, na.rm = TRUE)
  #     )

  #   stats$schooltype = paste0("schl_", stats$schooltype, "_orig")

  #   stats = tidyr::pivot_wider(stats,
  #     id_cols = c("DataZone"),
  #     names_from = c("schooltype"),
  #     names_glue = "{schooltype}_{.value}",
  #     values_from = names(stats)[3:ncol(stats)]
  #   )
  #   stats
  # }),
  # tar_target(school_stats_fastest, {
  #   make_school_stats(uptake_school_fastest, "fastest")
  # }),
  # tar_target(school_stats_quietest, {
  #   make_school_stats(uptake_school_quietest, "quietest")
  # }),
  # tar_target(school_stats_ebike, {
  #   make_school_stats(uptake_school_ebike, "ebike")
  # }),
  # tar_target(school_stats_balanced, {
  #   make_school_stats(uptake_school_balanced, "balanced")
  # }),
  # tar_target(school_stats_from_fastest, {
  #   make_school_stats_from(uptake_school_fastest, "fastest")
  # }),
  # tar_target(school_stats_from_quietest, {
  #   make_school_stats_from(uptake_school_quietest, "quietest")
  # }),
  # tar_target(school_stats_from_ebike, {
  #   make_school_stats_from(uptake_school_ebike, "ebike")
  # }),
  # tar_target(school_stats_from_balanced, {
  #   make_school_stats_from(uptake_school_balanced, "balanced")
  # }),

  # # Combine stats  ---------------------------------------------------------

  # tar_target(school_stats, {
  #   # Ebike routes for ebike scenario
  #   ebike = school_stats_ebike
  #   fastest = school_stats_fastest
  #   ebike = ebike[, !grepl("go_dutch", names(ebike))]
  #   # Can't use quietness/hilliness for ebike
  #   ebike = ebike[, !grepl("(quietness|hilliness)", names(ebike))]
  #   fastest = fastest[, !grepl("ebike", names(fastest))]
  #   names(ebike) = gsub("_ebike$", "_fastest", names(ebike))

  #   stats = dplyr::left_join(school_stats_baseline, fastest, by = "SeedCode")
  #   stats = dplyr::left_join(stats, ebike, by = "SeedCode")
  #   stats = dplyr::left_join(stats, school_stats_quietest, by = "SeedCode")
  #   stats
  # }),
  # tar_target(school_stats_from, {
  #   # Ebike routes for ebike scenario
  #   ebike = school_stats_from_ebike
  #   fastest = school_stats_from_fastest
  #   ebike = ebike[, !grepl("go_dutch", names(ebike))]
  #   # Can't use quietness/hilliness for ebike
  #   ebike = ebike[, !grepl("(quietness|hilliness)", names(ebike))]
  #   fastest = fastest[, !grepl("ebike", names(fastest))]
  #   names(ebike) = gsub("_ebike$", "_fastest", names(ebike))

  #   stats = dplyr::left_join(school_stats_from_baseline, fastest, by = "DataZone")
  #   stats = dplyr::left_join(stats, ebike, by = "DataZone")
  #   stats = dplyr::left_join(stats, school_stats_from_quietest, by = "DataZone")
  #   stats
  # }),
  # tar_target(commute_stats, {
  #   # Ebike routes for ebike scenario
  #   ebike = commute_stats_ebike
  #   fastest = commute_stats_fastest
  #   ebike = ebike[, !grepl("go_dutch", names(ebike))]
  #   # Can't use quietness/hilliness for ebike
  #   ebike = ebike[, !grepl("(quietness|hilliness)", names(ebike))]
  #   fastest = fastest[, !grepl("ebike", names(fastest))]
  #   names(ebike) = gsub("_ebike$", "_fastest", names(ebike))

  #   stats = dplyr::left_join(commute_stats_baseline, fastest, by = "DataZone")
  #   stats = dplyr::left_join(stats, ebike, by = "DataZone")
  #   stats = dplyr::left_join(stats, commute_stats_quietest, by = "DataZone")
  #   stats
  # }),
  # tar_target(zones_stats, {
  #   stats = dplyr::full_join(commute_stats, school_stats_from, by = "DataZone")
  #   stats = dplyr::full_join(stats, utility_stats, by = "DataZone")
  #   stats
  # }),

  # Now covered in build.R:
  # tar_target(zones_stats_json, {
  #   export_zone_json(zones_stats, "DataZone", path = region_folder)
  # }),

  # tar_target(school_stats_json, {
  #   export_zone_json(school_stats, "SeedCode", path = region_folder)
  # }),

  # Shopping OD ---------------------------------------------------------

  tar_target(trip_purposes, {
    trip_purposes = read.csv("./data-raw/scottish-household-survey-2012-19.csv")
    go_home = trip_purposes$Mean[trip_purposes$Purpose == "Go Home"]
    trip_purposes = trip_purposes |>
      filter(Purpose != "Sample size (=100%)") |>
      mutate(adjusted_mean = Mean / (sum(Mean) - go_home) * sum(Mean))
    trip_purposes
  }),
  tar_target(os_pois, {
    check = length(parameters)
    check = length(region_boundary)
    # Get shopping destinations from secure OS data
    path_teams = Sys.getenv("NPT_TEAMS_PATH")
    os_pois_raw = readRDS(file.path(path_teams, "secure_data/OS/os_poi.Rds"))
    os_pois_subset = os_pois_raw |>
      mutate(groupname = as.character(groupname))
    # os_pois_subset[region_boundary_buffered, ] |>
    os_pois_subset |>
      sf::st_transform("EPSG:27700")
  }),
  tar_target(grid, {
    grid = readRDS("./inputdata/grid_scot.Rds")
    grid = sf::st_transform(grid, "EPSG:4326")
    # grid = grid[region_boundary_buffered, ]
    grid |> sf::st_transform("EPSG:27700")
  }),
  tar_target(oas, {
    oas = readRDS("./inputdata/oas.Rds")
    # oas[region_boundary, ]
  }),
  tar_target(intermediate_zones, {
    izs = sf::read_sf("inputdata/SG_IntermediateZone_Bdry_2011.gpkg")
    # izs_centroids = sf::st_centroid(izs)
    # izs_centroids_within = izs_centroids[region_boundary |> sf::st_transform(27700), ]
    # izs[izs[[1]] %in% izs_centroids_within[[1]], ]
    izs
  }),

  # Utility OD -------------------------------------------------------------
  tar_target(od_shopping, {
    od_shopping = make_od(
      oas, os_pois, grid,
      purpose = "shopping",
      trip_purposes,
      zones, parameters
    )
    od_shopping
  }),
  tar_target(od_visiting, {
    od_visiting = make_od(
      oas, os_pois, grid,
      purpose = "visiting",
      trip_purposes,
      zones, parameters
    )
    od_visiting
  }),
  tar_target(od_leisure, {
    od_leisure = make_od(
      oas, os_pois, grid,
      purpose = "leisure",
      trip_purposes,
      zones, parameters
    )
    od_leisure
  }),

  # Combined utility trip purposes --------------------------------------------

  tar_target(od_utility_combined, {
    od_utility_combined = rbind(od_shopping, od_visiting, od_leisure) |>
      dplyr::slice_max(n = parameters$max_to_route, order_by = all, with_ties = FALSE)
    sum(od_utility_combined$bicycle) / sum(od_utility_combined$all)

    # Get % cycling for commuting per zone
    # pcycle_regional = sum(commute_stats$comm_orig_bicycle, na.rm = TRUE) /
    # sum(commute_stats$comm_orig_all, na.rm = TRUE)
    # pcycle_national = 0.016

    # commute_stats_minimal = commute_stats |>
    #   dplyr::select(DataZone, comm_orig_bicycle, comm_orig_all)
    # cycling_multiplier = commute_stats_minimal |>
    #   dplyr::transmute(
    #     DataZone,
    #     multiplier = (comm_orig_bicycle / comm_orig_all) /
    #       pcycle_national
    #   ) |>
    #   # 0 to 0.1:
    #   dplyr::mutate(multiplier = case_when(
    #     multiplier == 0 ~ 0.1,
    #     TRUE ~ multiplier
    #   ))
    # # summary(cycling_multiplier$multiplier)
    # # Add new cycling multiplier column to od_utility_combined
    # od_utility_combined = od_utility_combined |>
    #   dplyr::left_join(cycling_multiplier, by = join_by(geo_code1 == DataZone)) |>
    #   # Convert NAs to 1:
    #   dplyr::mutate(multiplier = case_when(
    #     is.na(multiplier) ~ 1,
    #     TRUE ~ multiplier
    #   )) |>
    #   dplyr::mutate(
    #     bicycle_new = bicycle * multiplier,
    #     car = car - (bicycle_new - bicycle),
    #     bicycle = bicycle_new
    #   ) |>
    #   dplyr::select(-multiplier, -bicycle_new)

    # Check new % cycling:
    # sum(od_utility_combined$bicycle) / sum(od_utility_combined$all)

    # Ensure the columns and distance units are identical to the other routing types
    # (apart from the additional trip purpose column)
    od_utility_combined = od_utility_combined |>
      dplyr::mutate(
        dist_euclidean = length_euclidean_unjittered * 1000,
        dist_euclidean_jittered = length_euclidean_jittered * 1000
      ) |>
      dplyr::select(
        geo_code1, geo_code2, car, foot, bicycle, all,
        dist_euclidean, public_transport, taxi, geometry,
        dist_euclidean_jittered, purpose
      )

    # Add Start and End DataZones for stats
    # geo_code1 and 2 refere to non-Data Zone ids
    end_point = lwgeom::st_endpoint(od_utility_combined)
    end_point = sf::st_join(sf::st_as_sf(end_point), zones)
    od_utility_combined$endDZ = end_point$DataZone

    start_point = lwgeom::st_startpoint(od_utility_combined)
    start_point = sf::st_join(sf::st_as_sf(start_point), zones)
    od_utility_combined$startDZ = start_point$DataZone

    od_utility_combined
  }),
  tar_target(
    save_od_datasets_to_csv,
    {
      # Commute:
      sf::write_sf(
        od_commute_subset,
        "outputdata/od_commute_subset.gpkg",
        delete_dsn = TRUE
      )
      write_csv(
        od_commute_subset |>
          sf::st_drop_geometry(),
        "outputdata/od_commute_subset.csv"
      )
      # School:
      sf::write_sf(
        od_school,
        "outputdata/od_school.gpkg",
        delete_dsn = TRUE
      )
      write_csv(
        od_school |>
          sf::st_drop_geometry(),
        "outputdata/od_school.csv"
      )
      # Utility:
      sf::write_sf(
        od_utility_combined,
        "outputdata/od_utility_combined.gpkg",
        delete_dsn = TRUE
      )
      write_csv(
        od_utility_combined |>
          sf::st_drop_geometry(),
        file = "outputdata/od_utility_combined.csv"
      )
      # Upload to nptscot/outputdata as od-data-2025-05 release:
      setwd("outputdata")
      # system("gh release create od-data-2025-05")
      # Upload csv files:
      system("gh release upload od-data-2025-05 od_utility_combined.csv od_commute_subset.csv od_school.csv --clobber")
      # Upload gpkg files:
      system("gh release upload od-data-2025-05 od_utility_combined.gpkg od_commute_subset.gpkg od_school.gpkg --clobber")
    }
  )
)
