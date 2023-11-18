# Instructions
# 1) Optional - install the latest packages using lines 12-15 below
# 2) library(targets)
# 3) Optional - to see real-time updates of progress
# tar_watch(seconds = 60, targets_only = TRUE)
# See the current status of the targets:
# tar_visnetwork(TRUE)
# 4) To run the build
# tar_make_future(workers = 4)
# If your RAM limited use tar_make() to run one job at a time

# Install packages required to define the if zonebuilder not installed:
pkgs_installed = "zonebuilder" %in% installed.packages() && 
  "odjitter" %in% installed.packages()
if (!pkgs_installed) {
  source("code/install.R")
}

library(tidyverse)
library(targets)
library(magrittr) # Light load of %>%
library(sf)
library(future) # Needed for multi-core running
library(future.callr)
library(stplanr)
library(geos)

tar_option_set(
  memory = "transient", 
  garbage_collection = TRUE,
  storage = "worker", 
  retrieval = "worker",
  # # packages that your targets need to run
  # packages = pkgs,
  format = "rds" # default storage format
)

# plan(callr)
tar_source()

# Targets -----------------------------------------------------------------
if(!file.exists("outputdata")){
  dir.create("outputdata")
}

list(
  # Detect when parameter file has changed:
  tar_target(name = param_file, command = "parameters.json", format = "file"),
  # Check Renviron exists, create output directory, load params:
  tar_target(parameters, {
    renviron_exists = file.exists(".Renviron")
    if(!renviron_exists) {
      warning("No .Renviron file, routing may not work")
    }
    p = jsonlite::read_json(param_file, simplifyVector = T)
    folder_name = paste0("outputdata/", p$date_routing)
    if(!dir.exists(folder_name)){
      dir.create(file.path(folder_name))
    }
    p 
  }),
  # Case study area:
  tar_target(study_area, {
    if(parameters$geo_subset) {
      if(parameters$open_data_build) {
        s_area = sf::read_sf("data-raw/study_area.geojson")
      } else {
        # Change the centrepoint and distance in km for other areas
        s_area = get_area("Forth Bridge", d = 20)
      }
    } else {
      s_area = NULL
    }
    s_area
  }),

  tar_target(zones, {
    if(parameters$open_data_build) {
      z = sf::read_sf("data-raw/DataZones.geojson")
    } else {
      z = readRDS("inputdata/DataZones.Rds") # 6976 zones
    }
    if(parameters$geo_subset) {
      z = z[study_area, op = sf::st_within]
    }
    z
  }),

  tar_target(od_data, {
    if(parameters$open_data_build) {
      od_raw = read_csv("data-raw/od_data_dz_synthetic.csv")
    } else {
      desire_lines_raw = read_TEAMS("secure_data/commute/commute_dl_sub30km.Rds")
      od_raw = as_tibble(sf::st_drop_geometry(desire_lines_raw))
    }  
    od_subset = od_raw %>%
      filter(geo_code1 %in% zones$DataZone) %>%
      filter(geo_code2 %in% zones$DataZone) %>%
      filter(dist_euclidean < 20000) %>%
      filter(dist_euclidean > 1000) %>%
      filter(all >= parameters$min_flow) %>% 
      mutate_od_commute()
    od_subset
  }),
  tar_target(subpoints_origins, {
    # source("data-raw/get_wpz.R")
    # sf::read_sf("data-raw/oas.geojson")
    if(parameters$open_data_build) {
      # create a sample of randomly located points in each zone: 
      spo = st_sample(zones, size = nrow(zones) * 20, by_polygon = TRUE)
    } else {
      spo = readRDS("inputdata/oas.Rds")
    }
    spo
  }),
  tar_target(subpoints_destinations, {
    # source("data-raw/get_wpz.R")
    # sf::read_sf("data-raw/workplaces_simple_edinburgh.geojson")
    #readRDS("inputdata/workplaces_simple.Rds") #Not enough points when using DataZones
    message("Getting destinations")
    spd = NULL
    if(parameters$open_data_build) {
      # message("Getting destinations for ", nrow(zones), " zones")
      # spd = st_sample(zones, size = nrow(zones) * 20, by_polygon = TRUE)
      spd = subpoints_origins
    } else {
      spd = read_TEAMS("secure_data/OS/os_poi.Rds")
      spd = spd[spd$workplace, ]
      
    }
    spd
  }),
  tar_target(od_commute_jittered, {
    # od_jittered = od_data # for no jittering
    # Install the Rust crate and the associated R package:
    # system("cargo install --git https://github.com/dabreegster/odjitter")
    z = zones
    z = z[subpoints_destinations, ]
    od = od_data |>
      filter(geo_code1 %in% z$DataZone) |>
      filter(geo_code2 %in% z$DataZone)
    set.seed(2023)
    # Test if cargo is available to system:
    source("R/is_bin_on_path.R")
    if (!is_bin_on_path("odjitter")) {
      old_path = Sys.getenv("PATH")
      Sys.setenv(PATH = paste(old_path, "/root/.cargo/bin", sep = ":"))
      odjitter_location = "/root/.cargo/bin/odjitter"
    } else {
      odjitter_location = "odjitter"
    }
    odj = odjitter::jitter(
      od = od,
      zones = z,
      subpoints_origins = subpoints_origins,
      subpoints_destinations = subpoints_destinations,
      disaggregation_threshold = 30,
      deduplicate_pairs = FALSE,
      odjitter_location = odjitter_location
    )
    odj$dist_euclidean_jittered = as.numeric(sf::st_length(odj))
    odj = odj %>%
      mutate(route_id = paste0(geo_code1, "_", geo_code2, "_", seq(nrow(odj))))
    # saveRDS(odj, "inputdata/od_commute_jittered.Rds")
    # Read in test OD dataset for package development:
    # sf::read_sf("https://github.com/nptscot/npt/releases/download/v1/od_jittered_demo.geojson")
  }),

  tar_target(od_commute_subset, {
    odcs = od_commute_jittered %>%
      filter(dist_euclidean_jittered < 16000) %>%
      filter(dist_euclidean_jittered > 1000) %>%
      slice_max(n = parameters$max_to_route, order_by = all, with_ties = FALSE)
    odcs
  }),
  

# School OD ---------------------------------------------------------------

tar_target(od_school, {
  # Get School OD
  if(parameters$open_data_build) {
    schools_dl = sf::read_sf("data-raw/school_desire_lines_open.geojson")
  } else {
    schools_dl = read_TEAMS("secure_data/schools/school_dl_sub30km.Rds")
  }
  if(parameters$geo_subset) {
    schools_dl = schools_dl[study_area, op = sf::st_within]
  }
  schools_dl$dist_euclidean_jittered = round(as.numeric(sf::st_length(schools_dl)))
  schools_dl = schools_dl %>%
    filter(dist_euclidean_jittered < 10000) %>%
    filter(dist_euclidean_jittered > 1000) %>%
    slice_max(order_by = all, n = parameters$max_to_route, with_ties = FALSE) %>% 
    mutate_od_school()
  schools_dl
}),

# School routing ----------------------------------------------------------

tar_target(rs_school_fastest, {
  f = paste0("outputdata/", parameters$date_routing, "routes_max_dist_school_fastest.Rds")
  if (file.exists(f)) {
    rs = readRDS(f)
  } else {
    rs = get_routes(od = od_school,
                    plans = "fastest", 
                    purpose = "school",
                    folder = paste0("outputdata/", parameters$date_routing),
                    date = parameters$date_routing,
                    segments = "both")
  }
  rs
}),

tar_target(done_school_fastest, {
  length(rs_school_fastest) #Hack for scheduling
}),

tar_target(rs_school_quietest, {
  length(done_school_fastest)
  f = paste0("outputdata/", parameters$date_routing, "routes_max_dist_school_quietest.Rds")
  if (file.exists(f)) {
    rs = readRDS(f)
  } else {
    rs = get_routes(od = od_school,
                    plans = "quietest", 
                    purpose = "school",
                    folder = paste0("outputdata/", parameters$date_routing),
                    date = parameters$date_routing,
                    segments = "both")
  }
  rs
}),

tar_target(done_school_quietest, {
  length(rs_school_quietest) #Hack for scheduling
}),

tar_target(rs_school_ebike, {
  length(done_school_quietest)
  f = paste0("outputdata/", parameters$date_routing, "routes_max_dist_school_ebike.Rds")
  if (file.exists(f)) {
    rs = readRDS(f)
  } else {
    rs = get_routes(od = od_school,
                    plans = "ebike", 
                    purpose = "school",
                    folder = paste0("outputdata/", parameters$date_routing),
                    date = parameters$date_routing,
                    segments = "both")
  }
  rs
}),

tar_target(done_school_ebike, {
  length(rs_school_ebike) #Hack for scheduling
}),

tar_target(rs_school_balanced, {
  length(done_commute_ebike)
  f = paste0("outputdata/", parameters$date_routing, "routes_max_dist_school_balanced.Rds")
  if (file.exists(f)) {
    rs = readRDS(f)
  } else {
    rs = get_routes(od = od_school,
                    plans = "balanced", 
                    purpose = "school",
                    folder = paste0("outputdata/", parameters$date_routing),
                    date = parameters$date_routing,
                    segments = "both")
  }
  rs
}),

tar_target(done_school_balanced, {
  length(rs_school_balanced) #Hack for scheduling
}),

# Commute routing ---------------------------------------------------------

tar_target(rs_commute_fastest, {
  length(done_school_ebike) # Do school routing first
  f = paste0("outputdata/", parameters$date_routing, "routes_max_dist_commute_fastest.Rds")
  if (file.exists(f)) {
    rs = readRDS(f)
  } else {
    rs = get_routes(od = od_commute_subset,
                    plans = "fastest", 
                    purpose = "commute",
                    folder = paste0("outputdata/", parameters$date_routing),
                    date = parameters$date_routing,
                    segments = "both")
  }
  rs
}),

tar_target(done_commute_fastest, {
  length(rs_commute_fastest) #Hack for scheduling
}),

tar_target(rs_commute_quietest, {
  length(done_commute_fastest)
  f = paste0("outputdata/", parameters$date_routing, "routes_max_dist_commute_quietest.Rds")
  if (file.exists(f)) {
    rs = readRDS(f)
  } else {
    rs = get_routes(od = od_commute_subset,
                    plans = "quietest", 
                    purpose = "commute",
                    folder = paste0("outputdata/", parameters$date_routing),
                    date = parameters$date_routing,
                    segments = "both")
  }
  rs
}),

tar_target(done_commute_quietest, {
  length(rs_commute_quietest) #Hack for scheduling
}),

tar_target(rs_commute_ebike, {
  length(done_commute_quietest)
  f = paste0("outputdata/", parameters$date_routing, "routes_max_dist_commute_ebike.Rds")
  if (file.exists(f)) {
    rs = readRDS(f)
  } else {
    rs = get_routes(od = od_commute_subset,
                    plans = "ebike", 
                    purpose = "commute",
                    folder = paste0("outputdata/", parameters$date_routing),
                    date = parameters$date_routing,
                    segments = "both")
  }
  rs
}),

tar_target(done_commute_ebike, {
  length(rs_commute_ebike) #Hack for scheduling
}),

tar_target(rs_commute_balanced, {
  length(done_school_balanced)
  f = paste0("outputdata/", parameters$date_routing, "routes_max_dist_commute_balanced.Rds")
  if (file.exists(f)) {
    rs = readRDS(f)
  } else {
    rs = get_routes(od = od_commute_subset,
                    plans = "balanced", 
                    purpose = "commute",
                    folder = paste0("outputdata/", parameters$date_routing),
                    date = parameters$date_routing,
                    segments = "both")
  }
  rs
}),

tar_target(done_commute_balanced, {
  length(rs_commute_balanced) #Hack for scheduling
}),


# Commute routing post-processing -----------------------------------------

tar_target(r_commute_fastest, {
  rs_commute_fastest[[1]]$routes
}),

tar_target(r_commute_quietest, {
  rs_commute_quietest[[1]]$routes
}),

tar_target(r_commute_ebike, {
  rs_commute_ebike[[1]]$routes
}),

tar_target(r_commute_balanced, {
  rs_commute_balanced[[1]]$routes
}),

tar_target(rnet_gq_commute_fastest, {
  segments2rnet(rs_commute_fastest[[1]]$segments)
}),

tar_target(rnet_gq_commute_quietest, {
  segments2rnet(rs_commute_quietest[[1]]$segments)
}),

tar_target(rnet_gq_commute_ebike, {
  segments2rnet(rs_commute_ebike[[1]]$segments)
}),

tar_target(rnet_gq_commute_balanced, {
  segments2rnet(rs_commute_balanced[[1]]$segments)
}),


# School routing post-processing -----------------------------------------

tar_target(r_school_fastest, {
  rs_school_fastest[[1]]$routes
}),

tar_target(r_school_quietest, {
  rs_school_quietest[[1]]$routes
}),

tar_target(r_school_ebike, {
  rs_school_ebike[[1]]$routes
}),

tar_target(r_school_balanced, {
  rs_school_balanced[[1]]$routes
}),

tar_target(rnet_gq_school_fastest, {
  segments2rnet(rs_school_fastest[[1]]$segments)
}),

tar_target(rnet_gq_school_quietest, {
  segments2rnet(rs_school_quietest[[1]]$segments)
}),

tar_target(rnet_gq_school_ebike, {
  segments2rnet(rs_school_ebike[[1]]$segments)
}),

tar_target(rnet_gq_school_balanced, {
  segments2rnet(rs_school_balanced[[1]]$segments)
}),

# Commute Uptake ----------------------------------------------------------

  tar_target(uptake_commute_fastest, {
    routes = r_commute_fastest %>%
      get_scenario_go_dutch()
    saveRDS(routes, "outputdata/routes_commute_fastest.Rds")
    routes
  }),
  
  tar_target(uptake_commute_quietest, {
    routes = r_commute_quietest %>%
      get_scenario_go_dutch()
    saveRDS(routes, "outputdata/routes_commute_quietest.Rds")
    routes
  }),
  
  tar_target(uptake_commute_ebike, {
    routes = r_commute_ebike %>%
      get_scenario_go_dutch()
    saveRDS(routes, "outputdata/routes_commute_ebike.Rds")
    routes
  }),
  
  tar_target(uptake_commute_balanced, {
    routes = r_commute_balanced %>%
      get_scenario_go_dutch()
    saveRDS(routes, "outputdata/routes_commute_balanced.Rds")
    routes
  }),
  
# School Uptake ----------------------------------------------------------

tar_target(uptake_school_fastest, {
  routes = r_school_fastest %>%
    get_scenario_go_dutch(purpose = "school")
  routes
}),

tar_target(uptake_school_quietest, {
  routes = r_school_quietest %>%
    get_scenario_go_dutch(purpose = "school")
  routes
}),

tar_target(uptake_school_ebike, {
  routes = r_school_ebike %>%
    get_scenario_go_dutch(purpose = "school")
  routes
}),

tar_target(uptake_school_balanced, {
  routes = r_school_balanced %>%
    get_scenario_go_dutch(purpose = "school")
  routes
}),


# Commute RNets -----------------------------------------------------------

tar_target(rnet_commute_fastest, {
  stplanr::overline2(uptake_commute_fastest, c("bicycle","bicycle_go_dutch","bicycle_ebike"))
}),

tar_target(rnet_commute_quietest, {
  stplanr::overline2(uptake_commute_quietest, c("bicycle","bicycle_go_dutch","bicycle_ebike"))
}),

tar_target(rnet_commute_ebike, {
  stplanr::overline2(uptake_commute_ebike, c("bicycle","bicycle_go_dutch","bicycle_ebike"))
}),

tar_target(rnet_commute_balanced, {
  stplanr::overline2(uptake_commute_balanced, c("bicycle","bicycle_go_dutch","bicycle_ebike"))
}),


# Primary School RNets -----------------------------------------------------------

tar_target(rnet_primary_fastest, {
  rnet = uptake_school_fastest
  rnet = rnet[rnet$schooltype == "primary",]
  rnet = stplanr::overline2(rnet, c("bicycle","bicycle_go_dutch","bicycle_ebike"))
  saveRDS(rnet, paste0("outputdata/rnet_primary_school_fastest.Rds"))
  rnet
}),

tar_target(rnet_primary_quietest, {
  rnet = uptake_school_quietest
  rnet = rnet[rnet$schooltype == "primary",]
  rnet = stplanr::overline2(rnet, c("bicycle","bicycle_go_dutch","bicycle_ebike"))
  saveRDS(rnet, paste0("outputdata/rnet_primary_school_quietest.Rds"))
  rnet
}),

tar_target(rnet_primary_ebike, {
  rnet = uptake_school_ebike
  rnet = rnet[rnet$schooltype == "primary",]
  rnet = stplanr::overline2(rnet, c("bicycle","bicycle_go_dutch","bicycle_ebike"))
  saveRDS(rnet, paste0("outputdata/rnet_primary_school_ebike.Rds"))
  rnet
}),

tar_target(rnet_primary_balanced, {
  rnet = uptake_school_balanced
  rnet = rnet[rnet$schooltype == "primary",]
  rnet = stplanr::overline2(rnet, c("bicycle","bicycle_go_dutch","bicycle_ebike"))
  saveRDS(rnet, paste0("outputdata/rnet_primary_school_balanced.Rds"))
  rnet
}),

# Secondary School RNets -----------------------------------------------------------

tar_target(rnet_secondary_fastest, {
  rnet = uptake_school_fastest
  rnet = rnet[rnet$schooltype == "secondary",]
  rnet = stplanr::overline2(rnet, c("bicycle","bicycle_go_dutch","bicycle_ebike"))
  saveRDS(rnet, paste0("outputdata/rnet_primary_school_fastest.Rds"))
  rnet
}),

tar_target(rnet_secondary_quietest, {
  rnet = uptake_school_quietest
  rnet = rnet[rnet$schooltype == "secondary",]
  rnet = stplanr::overline2(rnet, c("bicycle","bicycle_go_dutch","bicycle_ebike"))
  saveRDS(rnet, paste0("outputdata/rnet_primary_school_quietest.Rds"))
  rnet
}),

tar_target(rnet_secondary_ebike, {
  rnet = uptake_school_ebike
  rnet = rnet[rnet$schooltype == "secondary",]
  rnet = stplanr::overline2(rnet, c("bicycle","bicycle_go_dutch","bicycle_ebike"))
  saveRDS(rnet, paste0("outputdata/rnet_primary_school_ebike.Rds"))
  rnet
}),

tar_target(rnet_secondary_balanced, {
  rnet = uptake_school_balanced
  rnet = rnet[rnet$schooltype == "secondary",]
  rnet = stplanr::overline2(rnet, c("bicycle","bicycle_go_dutch","bicycle_ebike"))
  saveRDS(rnet, paste0("outputdata/rnet_primary_school_balanced.Rds"))
  rnet
}),

# Commute Zone stats ---------------------------------------------------------

tar_target(commute_stats_baseline, {
  stats = sf::st_drop_geometry(od_commute_subset)
  stats_from = dplyr::group_by(stats, geo_code1) %>%
    dplyr::summarise(all = sum(all, na.rm = TRUE),
                     bicycle = sum(bicycle, na.rm = TRUE),
                     car = sum(car, na.rm = TRUE),
                     foot = sum(foot, na.rm = TRUE),
                     public_transport = sum(public_transport, na.rm = TRUE),
                     taxi = sum(taxi, na.rm = TRUE))
  stats_to = dplyr::group_by(stats, geo_code2) %>%
    dplyr::summarise(all = sum(all, na.rm = TRUE),
                     bicycle = sum(bicycle, na.rm = TRUE),
                     car = sum(car, na.rm = TRUE),
                     foot = sum(foot, na.rm = TRUE),
                     public_transport = sum(public_transport, na.rm = TRUE),
                     taxi = sum(taxi, na.rm = TRUE))
  
  names(stats_from)[1] = "DataZone"
  names(stats_to)[1] = "DataZone"
  
  names(stats_from)[2:ncol(stats_from)] = paste0("comm_orig_",names(stats_from)[2:ncol(stats_from)])
  names(stats_to)[2:ncol(stats_to)] = paste0("comm_dest_",names(stats_to)[2:ncol(stats_to)])
  stats = dplyr::full_join(stats_from, stats_to, by = "DataZone")
  stats
}),

tar_target(commute_stats_fastest, {
  make_commute_stats(uptake_commute_fastest, "fastest")
}),

tar_target(commute_stats_quietest, {
  make_commute_stats(uptake_commute_quietest, "quietest")
}),

tar_target(commute_stats_ebike, {
  make_commute_stats(uptake_commute_ebike, "ebike")
}),

tar_target(commute_stats_balanced, {
  make_commute_stats(uptake_commute_balanced, "balanced")
}),


# School Zone stats ---------------------------------------------------------

tar_target(school_stats_baseline, {
  stats = sf::st_drop_geometry(od_school)
  stats = dplyr::group_by(stats, SeedCode, schooltype) %>%
    dplyr::summarise(all = sum(all, na.rm = TRUE),
                     bicycle = sum(bicycle, na.rm = TRUE),
                     car = sum(car, na.rm = TRUE),
                     foot = sum(foot, na.rm = TRUE),
                     public_transport = sum(public_transport, na.rm = TRUE),
                     other = sum(other, na.rm = TRUE))
  stats = tidyr::pivot_wider(stats, 
                             id_cols = c("SeedCode"),
                             names_from = c("schooltype"),
                             values_from = names(stats)[3:ncol(stats)])
  stats
}),

tar_target(school_stats_from_baseline, {
  stats = sf::st_drop_geometry(od_school)
  stats = dplyr::group_by(stats, DataZone, schooltype) %>%
    dplyr::summarise(all = sum(all, na.rm = TRUE),
                     bicycle = sum(bicycle, na.rm = TRUE),
                     car = sum(car, na.rm = TRUE),
                     foot = sum(foot, na.rm = TRUE),
                     public_transport = sum(public_transport, na.rm = TRUE),
                     other = sum(other, na.rm = TRUE))
  stats = tidyr::pivot_wider(stats, 
                             id_cols = c("DataZone"),
                             names_from = c("schooltype"),
                             values_from = names(stats)[3:ncol(stats)])
  stats
}),

tar_target(school_stats_fastest, {
  make_school_stats(uptake_school_fastest, "fastest")
}),

tar_target(school_stats_quietest, {
  make_school_stats(uptake_school_quietest, "quietest")
}),

tar_target(school_stats_ebike, {
  make_school_stats(uptake_school_ebike, "ebike")
}),

tar_target(school_stats_balanced, {
  make_school_stats(uptake_school_balanced, "balanced")
}),

tar_target(school_stats_from_fastest, {
  make_school_stats_from(uptake_school_fastest, "fastest")
}),

tar_target(school_stats_from_quietest, {
  make_school_stats_from(uptake_school_quietest, "quietest")
}),

tar_target(school_stats_from_ebike, {
  make_school_stats_from(uptake_school_ebike, "ebike")
}),

tar_target(school_stats_from_balanced, {
  make_school_stats_from(uptake_school_balanced, "balanced")
}),

# Combine stats  ---------------------------------------------------------

tar_target(school_stats, {
  # Ebike routes for ebike scenario
  ebike = school_stats_ebike
  fastest = school_stats_fastest
  ebike = ebike[,!grepl("go_dutch",names(ebike))]
  # Can't use quietness/hilliness for ebike
  ebike = ebike[,!grepl("(quietness|hilliness)",names(ebike))] 
  fastest = fastest[,!grepl("ebike",names(fastest))]
  names(ebike) = gsub("_ebike$","_fastest",names(ebike))
  
  stats = dplyr::left_join(school_stats_baseline, fastest, by = "SeedCode")
  stats = dplyr::left_join(stats, ebike, by = "SeedCode")
  stats = dplyr::left_join(stats, school_stats_quietest, by = "SeedCode")
  stats
}),

tar_target(school_stats_from, {
  # Ebike routes for ebike scenario
  ebike = school_stats_from_ebike
  fastest = school_stats_from_fastest
  ebike = ebike[,!grepl("go_dutch",names(ebike))]
  # Can't use quietness/hilliness for ebike
  ebike = ebike[,!grepl("(quietness|hilliness)",names(ebike))] 
  fastest = fastest[,!grepl("ebike",names(fastest))]
  names(ebike) = gsub("_ebike$","_fastest",names(ebike))
  
  stats = dplyr::left_join(school_stats_from_baseline, fastest, by = "DataZone")
  stats = dplyr::left_join(stats, ebike, by = "DataZone")
  stats = dplyr::left_join(stats, school_stats_from_quietest, by = "DataZone")
  stats
}),


tar_target(commute_stats, {
  # Ebike routes for ebike scenario
  ebike = commute_stats_ebike
  fastest = commute_stats_fastest
  ebike = ebike[,!grepl("go_dutch",names(ebike))]
  # Can't use quietness/hilliness for ebike
  ebike = ebike[,!grepl("(quietness|hilliness)",names(ebike))] 
  fastest = fastest[,!grepl("ebike",names(fastest))]
  names(ebike) = gsub("_ebike$","_fastest",names(ebike))
  
  stats = dplyr::left_join(commute_stats_baseline, fastest, by = "DataZone")
  stats = dplyr::left_join(stats, ebike, by = "DataZone")
  stats = dplyr::left_join(stats, commute_stats_quietest, by = "DataZone")
  stats
}),

tar_target(zones_stats, {
  dplyr::full_join(commute_stats, school_stats_from, by = "DataZone")
}),


tar_target(zones_stats_json, {
  export_zone_json(zones_stats, "DataZone", path = "outputdata")
}),

tar_target(school_stats_json, {
  export_zone_json(school_stats, "SeedCode", path = "outputdata")
}),



# Data Zone Maps ----------------------------------------------------------
tar_target(zones_contextual, {
  # Test that the SIMD data exists:
  f_simd = "inputdata/SIMD/simd2020_withgeog.zip"
  if (!file.exists(f_simd)) {
    message("SIMD data not found, skipping this target")
    return(NULL)
  }
  dir.create(file.path(tempdir(),"SIMD"))
  unzip(f_simd, exdir = file.path(tempdir(),"SIMD"))
  files = list.files(file.path(tempdir(),"SIMD/simd2020_withgeog"), full.names = TRUE)
  
  zones <- sf::read_sf(file.path(tempdir(),"SIMD/simd2020_withgeog/sc_dz_11.shp"))
  simd <- read_csv(file.path(tempdir(),"SIMD/simd2020_withgeog/simd2020_withinds.csv"))
  
  unlink(file.path(tempdir(),"SIMD"), recursive = TRUE)
  
  zones <- zones[,c("DataZone","Name","TotPop2011","ResPop2011","HHCnt2011")]
  simd$Intermediate_Zone <- NULL
  simd$Council_area  <- NULL
  
  zones <- dplyr::left_join(zones, simd, by = c("DataZone" = "Data_Zone"))
  zones <- sf::st_make_valid(zones)
  
  # Split into map
  zones <- zones[,c("DataZone","Total_population","SIMD2020v2_Decile",
                        "drive_petrol","drive_GP",
                        "drive_post","drive_primary","drive_retail",
                        "drive_secondary","PT_GP","PT_post",
                        "PT_retail","broadband")]
  zones <- sf::st_drop_geometry(zones)
  
  zones$drive_petrol <- round(zones$drive_petrol, 1)
  zones$drive_GP <- round(zones$drive_GP, 1)
  zones$drive_post <- round(zones$drive_post, 1)
  zones$drive_primary <- round(zones$drive_primary, 1)
  zones$drive_retail <- round(zones$drive_retail, 1)
  zones$drive_secondary <- round(zones$drive_secondary, 1)
  zones$PT_GP <- round(zones$PT_GP, 1)
  zones$PT_post <- round(zones$PT_post, 1)
  zones$PT_retail <- round(zones$PT_retail, 1)
  zones$broadband <- as.integer(gsub("%","",zones$broadband))
  
  zones
}),

tar_target(zones_tile, {
  if (is.null(zones_contextual)) {
    return(NULL)
  }
  z = zones
  z = z[,"DataZone"]
  z = dplyr::left_join(z, zones_contextual, by = "DataZone")
  
  zs = zones_stats[,c("DataZone","comm_orig_all","comm_orig_bicycle","comm_orig_bicycle_go_dutch_fastest")]
  zs$pcycle = round(zs$comm_orig_bicycle / zs$comm_orig_all * 100)
  zs$pcycle_go_dutch = round(zs$comm_orig_bicycle_go_dutch_fastest / zs$comm_orig_all * 100)
  zs$pcycle[is.na(zs$pcycle)] = 0
  zs$pcycle_go_dutch[is.na(zs$pcycle_go_dutch)] = 0
  
  zs = zs[,c("DataZone","pcycle","pcycle_go_dutch")]
  
  z = dplyr::left_join(z, zs, by = "DataZone")
  
  z$area = as.numeric(st_area(z)) / 10000
  z$population_density = round(z$Total_population / z$area)
  z$area = NULL
  
  make_geojson_zones(z, "outputs/data_zones.geojson")
  
  z
}),

tar_target(zones_dasymetric_tile, {
  
  if (parameters$open_data_build){
    return(NULL)
  }
  
  b_verylow = read_TEAMS("open_data/os_buildings/buildings_low_nat_lsoa_split.Rds")
  b_low = read_TEAMS("open_data/os_buildings/buildings_low_reg_lsoa_split.Rds")
  b_med = read_TEAMS("open_data/os_buildings/buildings_med_lsoa_split.Rds")
  b_high = read_TEAMS("open_data/os_buildings/buildings_high_lsoa_split.Rds")
  
  zones = sf::st_drop_geometry(zones_tile)
  
  b_verylow = dplyr::left_join(b_verylow, zones, by = c("geo_code" = "DataZone"))
  b_low = dplyr::left_join(b_low, zones, by = c("geo_code" = "DataZone"))
  b_med = dplyr::left_join(b_med, zones, by = c("geo_code" = "DataZone"))
  b_high = dplyr::left_join(b_high, zones, by = c("geo_code" = "DataZone"))
  
  make_geojson_zones(b_verylow, "outputs/dasymetric_verylow.geojson")
  make_geojson_zones(b_low, "outputs/dasymetric_low.geojson")
  make_geojson_zones(b_med, "outputs/dasymetric_med.geojson")
  make_geojson_zones(b_high, "outputs/dasymetric_high.geojson")
  
  TRUE
}),


tar_target(school_points, {
  schools = sf::read_sf("inputdata/Schools/school_locations.geojson")
  make_geojson_zones(schools, "outputs/school_locations.geojson")
  schools
}),

# Combine networks ---------------------------------------------------------

tar_target(combined_network, {
    
    # Purpose: Combine individual rnets into single rnet -----------------------
    rnet_cl = list(fastest = rnet_commute_fastest,
                   quietest = rnet_commute_quietest,
                   ebike = rnet_commute_ebike)
    
    rnet_sl_p = list(fastest = rnet_primary_fastest,
                     quietest = rnet_primary_quietest,
                     ebike = rnet_primary_ebike)
    
    rnet_sl_s = list(fastest = rnet_secondary_fastest,
                     quietest = rnet_secondary_quietest,
                     ebike = rnet_secondary_ebike)
    
    rnet_quietness = list(rnet_gq_school_fastest,
                          rnet_gq_school_quietest,
                          rnet_gq_school_ebike,
                          rnet_gq_commute_fastest,
                          rnet_gq_commute_quietest,
                          rnet_gq_commute_ebike)
    names(rnet_cl) = paste0("commute_", names(rnet_cl))
    names(rnet_sl_p) = paste0("primary_", names(rnet_sl_p))
    names(rnet_sl_s) = paste0("secondary_", names(rnet_sl_s))
    
    rnl = c(rnet_cl, rnet_sl_p, rnet_sl_s, list(rnet_quietness))
    
    rnet_combined = combine_rnets(rnl = rnl,
                                  ncores = 1, 
                                  regionalise = 1e5,
                                  add_all = TRUE)
    
    # Round values
    rnet_combined[grepl("bicycle", names(rnet_combined))] = lapply(sf::st_drop_geometry(rnet_combined[grepl("bicycle", names(rnet_combined))]), round)
    
    
    # Sort rnet for tiling, low values drawn first
    rnet_combined = rnet_combined[order(rnet_combined$all_fastest_bicycle_go_dutch, 
                                        rnet_combined$all_quietest_bicycle_go_dutch),]
    
    rnet_combined
  }),
  
  tar_target(combined_network_tile, {
    # Not All Data is put into the UI
    rnet_tile = combined_network
    
    # Only use ebike routing for ebike scenario
    nms_noebike = !grepl("(ebike)",names(rnet_tile)) #keep all non-ebike
    nms_ebike2 = grepl("ebike.*ebike",names(rnet_tile)) # keep ebike twice
    nms_ebike1 = grepl("ebike",names(rnet_tile)) # keep quietest ebike
    nms_ebike1 = nms_ebike1 & !nms_ebike2
    nms_ebike1 = nms_ebike1 & grepl("quietest",names(rnet_tile))
    
    rnet_tile = rnet_tile[,nms_noebike | nms_ebike2 | nms_ebike1]
    names(rnet_tile) = gsub("_ebike_","_fastest_",names(rnet_tile))
    
    #Order Variables
    nms_end = c("Gradient","Quietness","geometry" )
    nms = names(rnet_tile)[!names(rnet_tile) %in% nms_end]
    rnet_tile = rnet_tile[c(nms[order(nms)], nms_end)]
    
    make_geojson_zones(rnet_tile, "outputdata/combined_network_tile.geojson")
    
    rnet_tile
  }),
  

# Make PMTiles for website ------------------------------------------------

tar_target(pmtiles_school, {
  check = length(school_points)
  command_tippecanoe = paste('tippecanoe -o schools.pmtiles',
                             '--name=schools',
                             '--layer=schools',
                             '--attribution=UniverstyofLeeds',
                             '--minimum-zoom=6',
                             '--maximum-zoom=13',
                             '--maximum-tile-bytes=5000000',
                             '--simplification=10',
                             '--buffer=5',
                             '-rg4',
                             '--force  school_locations.geojson', collapse = " ")
  
  if(.Platform$OS.type == "unix") {
    command_cd = 'cd outputdata'
    command_all = paste(c(command_cd, command_tippecanoe), collapse = "; ")
  } else {
    # Using WSL
    dir = getwd()
    command_start = 'bash -c '
    command_cd = paste0('cd /mnt/',tolower(substr(dir,1,1)),substr(dir,3,nchar(dir)),'/outputs')
    command_all = paste(c(command_cd, command_tippecanoe), collapse = "; ")
    command_all = paste0(command_start,'"',command_all,'"')
  }
  responce = system(command_all, intern = TRUE)
  responce
}),


tar_target(pmtiles_zones, {
  check = length(pmtiles_rnet)
  command_tippecanoe = paste('tippecanoe -o data_zones.pmtiles',
                             '--name=data_zones',
                             '--layer=data_zones',
                             '--attribution=UniverstyofLeeds',
                             '--minimum-zoom=6',
                             '-zg',
                             '--coalesce-smallest-as-needed',
                             '--detect-shared-borders',
                             '--extend-zooms-if-still-dropping',
                             '--maximum-tile-bytes=5000000',
                             '--simplification=10',
                             '--buffer=5',
                             '--force  data_zones.geojson', collapse = " ")
  
  if(.Platform$OS.type == "unix") {
    command_cd = 'cd outputdata'
    command_all = paste(c(command_cd, command_tippecanoe), collapse = "; ")
  } else {
    # Using WSL
    dir = getwd()
    command_start = 'bash -c '
    command_cd = paste0('cd /mnt/',tolower(substr(dir,1,1)),substr(dir,3,nchar(dir)),'/outputs')
    command_all = paste(c(command_cd, command_tippecanoe), collapse = "; ")
    command_all = paste0(command_start,'"',command_all,'"')
  }
  responce = system(command_all, intern = TRUE)
  responce
}),

tar_target(pmtiles_buildings, {
    check = length(pmtiles_rnet)
 
  tippecanoe_verylow = paste('tippecanoe -o dasymetric_verylow.pmtiles',
                             '--name=dasymetric',
                             '--layer=dasymetric',
                             '--attribution=OS',
                             '--minimum-zoom=4',
                             '--maximum-zoom=6',
                             '--coalesce-smallest-as-needed',
                             '--detect-shared-borders',
                             '--maximum-tile-bytes=5000000',
                             '--simplification=1',
                             '--buffer=5',
                             '--force dasymetric_verylow.geojson', 
                             collapse = " ")
  
  tippecanoe_low = paste('tippecanoe -o dasymetric_low.pmtiles',
                         '--name=dasymetric',
                         '--layer=dasymetric',
                         '--attribution=OS',
                         '--minimum-zoom=7',
                         '--maximum-zoom=9',
                         '--coalesce-smallest-as-needed',
                         '--detect-shared-borders',
                         '--maximum-tile-bytes=5000000',
                         '--simplification=1',
                         '--buffer=5',
                         '--force dasymetric_low.geojson', 
                         collapse = " ")
  
  tippecanoe_med = paste('tippecanoe -o dasymetric_med.pmtiles',
                         '--name=dasymetric',
                         '--layer=dasymetric',
                         '--attribution=OS',
                         '--minimum-zoom=10',
                         '--maximum-zoom=14',
                         '--coalesce-smallest-as-needed',
                         '--detect-shared-borders',
                         '--maximum-tile-bytes=5000000',
                         '--simplification=2',
                         '--buffer=5',
                         '--force dasymetric_med.geojson', 
                         collapse = " ")
  
  tippecanoe_high = paste('tippecanoe -o dasymetric_high.pmtiles',
                          '--name=dasymetric',
                          '--layer=dasymetric',
                          '--attribution=OS',
                          '-zg',
                          '--minimum-zoom=15',
                          '--extend-zooms-if-still-dropping',
                          '--coalesce-smallest-as-needed',
                          '--detect-shared-borders',
                          '--maximum-tile-bytes=5000000',
                          '--simplification=5',
                          '--buffer=5',
                          '--force dasymetric_high.geojson', 
                          collapse = " ")
  
  tippecanoe_join = paste('tile-join -o dasymetric.pmtiles -pk --force',
                          'dasymetric_verylow.pmtiles',
                          'dasymetric_low.pmtiles',
                          'dasymetric_med.pmtiles',
                          'dasymetric_high.pmtiles', 
                          collapse = " ")
  
  
  
  if(.Platform$OS.type == "unix") {
    command_cd = 'cd outputdata'
    command_all = paste(c(command_cd, tippecanoe_verylow, tippecanoe_low, 
                          tippecanoe_med, tippecanoe_high, tippecanoe_join), collapse = "; ")
  } else {
    # Using WSL
    dir = getwd()
    command_start = 'bash -c '
    command_cd = paste0('cd /mnt/',tolower(substr(dir,1,1)),substr(dir,3,nchar(dir)),'/outputs')
    command_all = paste(c(command_cd, tippecanoe_verylow, tippecanoe_low, 
                          tippecanoe_med, tippecanoe_high, tippecanoe_join), collapse = "; ")
    command_all = paste0(command_start,'"',command_all,'"')
  }
  responce = system(command_all, intern = TRUE)
  responce
}),

tar_target(pmtiles_rnet, {
  check = length(combined_network_tile)
  command_tippecanoe = paste('tippecanoe -o rnet.pmtiles',
                             '--name=rnet',
                             '--layer=rnet',
                             '--attribution=UniverstyofLeeds',
                             '--minimum-zoom=6',
                             '--maximum-zoom=13',
                             '--drop-smallest-as-needed',
                             '--maximum-tile-bytes=5000000',
                             '--simplification=10',
                             '--buffer=5',
                             '--force  combined_network_tile.geojson', collapse = " ")
  
  if(.Platform$OS.type == "unix") {
    command_cd = 'cd outputdata'
    command_all = paste(c(command_cd, command_tippecanoe), collapse = "; ")
  } else {
    # Using WSL
    dir = getwd()
    command_start = 'bash -c '
    command_cd = paste0('cd /mnt/',tolower(substr(dir,1,1)),substr(dir,3,nchar(dir)),'/outputdata')
    
    command_all = paste(c(command_cd, command_tippecanoe), collapse = "; ")
    command_all = paste0(command_start,'"',command_all,'"')
  }
  responce = system(command_all, intern = TRUE)
  responce
}),
  
  tar_target(save_outputs, {
    check = length(pmtiles_buildings)
    check = length(rnet_commute_balanced)
    check = length(zones_dasymetric_tile)
    check = length(pmtiles_rnet)
    check = length(pmtiles_buildings)

    message("Saving outputs for ", parameters$date_routing)
    
    saveRDS(od_commute_subset, "outputdata/od_commute_subset.Rds")
    saveRDS(zones_stats, "outputdata/zones_stats.Rds")
    saveRDS(school_stats, "outputdata/school_stats.Rds")
    sf::write_sf(simplify_network, "outputdata/simplified_network.geojson")
    
    file.copy("outputs/daysmetric.pmtiles","outputdata/daysmetric.pmtiles")
    file.copy("outputs/data_zones.pmtiles","outputdata/data_zones.pmtiles")
    file.copy("outputs/rnet.pmtiles","outputdata/rnet.pmtiles")
    file.copy("outputs/schools.pmtiles","outputdata/schools.pmtiles")

    sys_time = Sys.time()
    zip(zipfile = "outputdata/combined_network_tile.zip", "outputdata/combined_network_tile.geojson")
    file.remove("outputdata/combined_network_tile.geojson")
    # Upload pmtiles to release
    # cd outputdata
    # gh release upload z2023-07-28 rnet_2023-07-04.pmtiles
    # cd ..
    sys_time
  }),
  
  # tar_target(visualise_rnet, {
  #   # tar_source("code/vis_network.R")
  #   # tarchetypes::tar_
  # }),
  # tarchetypes::tar_render(visualise_rnet, path = "code/vis_network.Rmd", params = list(rnet_commute_list)),
  
  # tarchetypes::tar_render(report, path = "README.Rmd", params = list(zones, rnet)),
  tar_target(upload_data, {
    
    # Ensure the target runs after
    check = length(save_outputs)

    commit = gert::git_log(max = 1)
    message("Commit: ", commit)
    full_build = 
      # isFALSE(parameters$geo_subset) &&     
      isFALSE(parameters$open_data_build) &&
      parameters$max_to_route > 20e3
    is_linux = Sys.info()[['sysname']] == "Linux"
    if(full_build) {
    v = paste0("v", save_outputs, "_commit_", commit$commit)
    v = gsub(pattern = " |:", replacement = "-", x = v)
    setwd("outputdata")
    f = list.files(path = ".", pattern = "Rds|zip|pmtiles|.json")
    # Piggyback fails with error message so commented and using cust
    # piggyback::pb_upload(f)
    msg = glue::glue("gh release create {v} --generate-notes")
    message("Creating new release and folder to save the files: ", v)
    dir.create(v)
    message("Going to try to upload the following files: ", paste0(f, collapse = ", "))
    message("With sizes: ", paste0(fs::file_size(f), collapse = ", "))
    system(msg)
    for(i in f) {
      gh_release_upload(file = i, tag = v)
      # Move into a new directory
      file.copy(from = i, to = file.path(v, i))
    }
    message("Files stored in output folder: ", v)
    message("Which contains: ", paste0(list.files(v), collapse = ", "))
    # For specific version:
    # system("gh release create v0.0.1 --generate-notes")
    file.remove(f)
    setwd("..")
  }  else {
    message("Not full build or gh command line tool not available")
    message("Not uploading files: manually move contents of outputdata (see upload_data target for details)")
  }
  Sys.Date()
  }),
  
  tar_target(metadata, {
    upload_data
    # metadata_all = tar_meta()
    # metadata_targets = metadata_all %>% 
    #   filter(type == "stem")
    # readr::write_csv(metadata_targets, "outputs/metadata_targets.csv")
    
    # TODO: add more columns
    build_summary = tibble::tibble(
      n_segment_cells = nrow(combined_network) * ncol(combined_network),
      min_flow = parameters$min_flow,
      max_to_route = parameters$max_to_route,
      # time_total_mins = round(sum(metadata_targets$seconds) / 60, digits = 2),
      # time_r_commute_mins = round(metadata_targets %>% 
      #                               filter(name == "r_commute") %>% 
      #                               pull(seconds) / 60, 
      #                             digits = 2),
      routing_date = get_routing_date()
    )
    # # To overwrite previous build summary:
    # write_csv(build_summary, "outputs/build_summary.csv")
    if (file.exists("outputs/build_summary.csv")) {
      build_summary_previous = readr::read_csv("outputs/build_summary.csv")
    } else {
      build_summary_previous = NULL
    }
    # Combine previous and current build datasets
    build_summary = data.table::rbindlist(list(build_summary, build_summary_previous), fill = TRUE)
    write_csv(build_summary, "outputs/build_summary.csv")
  }),

  tar_target(simplify_network, {
    cue = tar_cue(mode = "always")
    # Read spatial data directly from URLs into sf objects
    # TODO: use small dataset if open data build is TRUE
    if (parameters$open_data_build) {
      rnet_x = sf::read_sf("https://github.com/ropensci/stplanr/releases/download/v1.0.2/rnet_x_ed.geojson")
      rnet_x_buffers = st_buffer(rnet_x, dist = 20, endCapStyle = "FLAT")
      single_rnet_x_buffer = st_union(rnet_x_buffers)
      rnet_x_buffer = st_sf(geometry = single_rnet_x_buffer)
      rnet_x_buffer = st_make_valid(rnet_x_buffer)
    } else {
      # URL for the original route network
      url_rnet_x = "https://github.com/nptscot/networkmerge/releases/download/v0.1/OS_Scotland_Network.geojson"
      f_rnet_x = basename(url_rnet_x)
      if (!file.exists(f_rnet_x)) { 
          download.file(url_rnet_x, f_rnet_x, method = "libcurl")
      }
      if (file.exists(f_rnet_x) && file.size(f_rnet_x) > 0) {
          rnet_x = sf::read_sf(f_rnet_x)
      } else {
          stop("File download failed or file is empty for rnet_x")
      }

    }
    
    # Assign rnet_y from combined_network
    rnet_y = combined_network

    # Transform the spatial data to a different coordinate reference system (EPSG:27700)
    # TODO: uncomment:
    rnet_xp = st_transform(rnet_x, "EPSG:27700")
    rnet_yp = st_transform(rnet_y, "EPSG:27700")

    # Extract column names from the rnet_yp
    name_list = names(rnet_yp)

    # Initialize an empty list
    funs = list()

    # Loop through each name and assign it a function based on specific conditions
    for (name in name_list) {
      if (name == "geometry") {
        next  # Skip the current iteration
      } else if (name %in% c("Gradient", "Quietness")) {
        funs[[name]] = mean
      } else {
        funs[[name]] = sum
      }
    }

    # Merge the spatial objects rnet_xp and rnet_yp based on specified parameters
    dist = 20
    angle = 10
    rnet_merged_all = stplanr::rnet_merge(rnet_xp, rnet_yp, dist = dist, segment_length = 10, funs = funs, max_angle_diff = 20)  # 

    # Remove specific columns from the merged spatial object
    rnet_merged_all = rnet_merged_all[ , !(names(rnet_merged_all) %in% c('identifier','length_x'))]

    # Remove Z and M dimensions (if they exist) and set geometry precision
    has_Z_or_M = any(st_dimension(rnet_merged_all) %in% c("XYZ", "XYM", "XYZM"))

    # If Z or M dimensions exist, remove them and print a message
    if (has_Z_or_M) {
      rnet_merged_all = st_zm(rnet_merged_all, what = "ZM")
      cat("Z or M dimensions have been removed from rnet_merged_all.\n")
    }

    # Set the precision of geometries in 'rnet_merged_all' to 1e3 (0.001)
    rnet_merged_all$geometry = st_set_precision(rnet_merged_all$geometry, 1e3)

    # Round all numeric columns in 'rnet_merged_all' to 0 decimal places
    rnet_merged_all = rnet_merged_all %>%
      mutate(across(where(is.numeric), ~ round(.x, 0)))      

    # Prepare a list of columns to check for NA, excluding 'geometry'
    rnet_yp_list = as.list(names(rnet_yp))
    columns_to_check = unlist(rnet_yp_list[rnet_yp_list != "geometry"])

    # Filter out rows in 'rnet_merged_all' where all specified columns are NA
    rnet_merged_all = rnet_merged_all %>%
      filter(rowSums(is.na(select(., all_of(columns_to_check)))) != length(columns_to_check))

    # Selecting only the geometry column from the 'rnet_merged_all' dataset.
    rnet_merged_all_only_geometry = rnet_merged_all %>% select(geometry)

    # Merging all geometries into a single geometry using st_union from the sf package.
    rnet_merged_all_union = sf::st_union(rnet_merged_all_only_geometry)

    # Transforming the merged geometry to a specific coordinate reference system (CRS), EPSG:27700.
    rnet_merged_all_projected = sf::st_transform(rnet_merged_all_union, "EPSG:27700")

    # Converting the projected geometry into a GEOS geometry. GEOS is a library used for spatial operations.
    rnet_merged_all_geos = geos::as_geos_geometry(rnet_merged_all_projected)

    # Creating a buffer around the GEOS geometry. This expands the geometry by a specified distance (in meters).
    rnet_merged_all_geos_buffer = geos::geos_buffer(rnet_merged_all_geos, distance = 30)

    # Converting the buffered GEOS geometry back to an sf object.
    rnet_merged_all_projected_buffer = sf::st_as_sf(rnet_merged_all_geos_buffer)

    # Confirming buffered geometry CRS as EPSG:27700.
    rnet_merged_all_buffer = sf::st_transform(rnet_merged_all_projected_buffer, "EPSG:4326")

    # Subsetting another dataset 'rnet_y' based on the spatial relation with 'rnet_merged_all_buffer'.
    # It selects features from 'rnet_y' that are within the boundaries of 'rnet_merged_all_buffer'.
    rnet_y_subset = rnet_y[rnet_merged_all_buffer, , op = sf::st_within]

    # Filter 'rnet_y' to exclude geometries within 'within_join'
    rnet_y_rest = rnet_y[!rnet_y$geometry %in% rnet_y_subset$geometry, ]

    # Transform the CRS of the 'rnet_merged_all' object to WGS 84 (EPSG:4326)  
    rnet_merged_all = sf::st_transform(rnet_merged_all, "EPSG:4326")

    # Combine 'rnet_y_rest' and 'rnet_merged_all' into a single dataset
    simplified_network = bind_rows(rnet_y_rest, rnet_merged_all)

    # Remove specified columns and replace NA values with 0 in the remaining columns
    items_to_remove = c('geometry', 'length_x_original', 'length_x_cropped')

    cols_to_convert = names(simplified_network)[!names(simplified_network) %in% items_to_remove]
    for (col in cols_to_convert) {
      simplified_network[[col]][is.na(simplified_network[[col]])] = 0
    }

    simplified_network
  })

  # The code below is using python script to acheive the same funtion as the R code (lines 1303 - 1344) above
  # tar_target(rnet_simple, {
  #     # Run this target only after the 'simplify_network' target has been run:
  #     simplify_network
  #     # Get the path to the Python executable using 'where python'
  #     python_path = system("where python", intern = TRUE)[1]

  #     # Get the current working directory
  #     current_wd = getwd()

  #     # Define the relative path to the directory containing the Python script
  #     relative_script_path = "code/sjoin_rnet.py"

  #     # Construct the full path to the Python script using the current working directory
  #     full_script_path = file.path(current_wd, relative_script_path)

  #     # Construct the command to run the Python script
  #     cmd = paste(python_path, full_script_path)

  #     # Run the Python script using the system function
  #     system(cmd)
      
  #     # Read the output from the Python script
  #     sf::st_read("tmp/simplified_network.gpkg")
  # })
)
# # Download a snapshot of the data:
# setwd("outputdata")
# system("gh release download v2023-03-24-22-28-51_commit_e2a60d0f06e6ddbf768382b19dc524cb3824c0c4 ")
