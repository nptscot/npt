# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
remotes::install_dev("cyclestreets")
remotes::install_github("dabreegster/odjitter", subdir = "r")
remotes::install_cran("targets")
library(targets)
library(tidyverse)
# library(tmap)
library(stplanr)
library(sf)

# Set target options:
tar_option_set(
  memory = "transient", garbage_collection = TRUE,
  packages = c("tibble","zonebuilder","dplyr","stplanr","lubridate",
               "cyclestreets","odjitter","stringr","sf","tidyr","data.table",
               "glue","zip","jsonlite","remotes","gert","collapse",
               "pct"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)
# # Remove previous targets objects:
# tar_destroy()

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
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
    folder = paste0("outputdata/", p$date_routing)
    if(!dir.exists(folder)){
      dir.create(file.path(folder))
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
    #desire_lines_raw = readRDS("inputdata/desire_lines_scotland.Rds")
    path_teams = Sys.getenv("NPT_TEAMS_PATH")
    if(nchar(path_teams) == 0){
      stop("Can't find Teams folder of secure data. Use usethis::edit_r_environ() to define NPT_TEAMS_PATH ")
    }
    if(file.exists(file.path(path_teams,"secure_data/commute/commute_dl_sub30km.Rds"))){
      desire_lines_raw = readRDS(file.path(path_teams, "secure_data/commute/commute_dl_sub30km.Rds"))
      od_raw = as_tibble(sf::st_drop_geometry(desire_lines_raw))
    } else {
      stop("Can't find ",file.path(path_teams,"secure_data/commute/commute_dl_sub30km.Rds"))
    }
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
      path_teams = Sys.getenv("NPT_TEAMS_PATH")
      if(nchar(path_teams) == 0){
        stop("Can't find Teams folder of secure data. Use usethis::edit_r_environ() to define NPT_TEAMS_PATH ")
      }
      spd = readRDS(file.path(path_teams,"secure_data/OS/os_poi.Rds"))
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
    odj = odjitter::jitter(
      od = od,
      zones = z,
      subpoints_origins = subpoints_origins,
      subpoints_destinations = subpoints_destinations,
      disaggregation_threshold = 30,
      deduplicate_pairs = FALSE
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
  
  tar_target(rnet_commute_list, {
    message(parameters$date_routing)
    message("Calculating ", nrow(od_commute_subset), " routes")
    folder = paste0("outputdata/", parameters$date_routing)
    rnet_commute_list = sapply(parameters$plans, function(x) NULL)
    
    p = "fastest" # for testing
    for(p in parameters$plans) {
      routes_commute = get_routes(
        od = od_commute_subset,
        plans = p,
        folder = folder,
        date = parameters$date_routing,
        batch_save = TRUE
      )
      routes_commute = routes_commute[[p]] %>%
        get_scenario_go_dutch() %>%
        as_tibble()
      
      # # TODO: Remove if not needed:
      routes_commute[["geometry"]] = st_sfc(routes_commute[["geometry"]], recompute_bbox = TRUE)
      routes_commute = st_as_sf(routes_commute)
      saveRDS(routes_commute, paste0("outputdata/uptake_commute_", p, ".Rds"))
      
      message("Building Commute ", p, " network")
      rnet = make_rnets(routes_commute, ncores = 1)
      
      f = paste0("outputdata/rnet_commute_", p, ".Rds")
      # saveRDS(rnet, f)
      rnet_commute_list[[p]] = rnet
    }
    rnet_commute_list
  }),
  
  tar_target(r_school, {
    # Get School OD
    if(parameters$open_data_build) {
      schools_dl = sf::read_sf("data-raw/school_desire_lines_open.geojson")
    } else {
      path_teams = Sys.getenv("NPT_TEAMS_PATH")
      if(nchar(path_teams) == 0){
        stop("Can't find Teams folder of secure data. Use usethis::edit_r_environ() to define NPT_TEAMS_PATH ")
      }
      if(file.exists(file.path(path_teams,"secure_data/schools/school_dl_sub30km.Rds"))){
        schools_dl = readRDS(file.path(path_teams, "secure_data/schools/school_dl_sub30km.Rds"))
      } else {
        stop("Can't find ",file.path(path_teams,"secure_data/schools/school_dl_sub30km.Rds"))
      }
    }
    if(parameters$geo_subset) {
      schools_dl = schools_dl[study_area, op = sf::st_within]
    }
    schools_dl$dist_euclidean_jittered = round(as.numeric(sf::st_length(schools_dl)))
    schools_dl = schools_dl %>%
      filter(dist_euclidean_jittered < 10000) %>%
      filter(dist_euclidean_jittered > 1000) %>%
      slice_max(order_by = count, n = parameters$max_to_route, with_ties = FALSE) %>% 
      mutate_od_school()
    folder = paste0("outputdata/", parameters$date_routing)
    routes_school = get_routes(
      schools_dl,
      plans = parameters$plans, purpose = "school",
      folder = folder,
      nrow_batch = 100000,
      date = parameters$date_routing,
      batch_save = TRUE
    )
    routes_school
  }),
  
  tar_target(uptake_list_school, {
    #p = "balanced"
    uptake_list_school = lapply(parameters$plan, function(p) {
      message("Uptake for ", p, " school routes")
      names(r_school[[1]])
      routes = r_school[[p]] %>%
        mutate(all = count) %>% 
        get_scenario_go_dutch(purpose = "school") %>%
        as_tibble()
      routes[["geometry"]] = st_sfc(routes[["geometry"]], recompute_bbox = TRUE)
      routes = st_as_sf(routes)
      routes
    })
    names(uptake_list_school) = parameters$plans
    uptake_list_school
  }),
  
  tar_target(rnet_school_list, {
    
    # Primary
    rnet_primary_list = sapply(parameters$plans, function(x) NULL)
    for(p in parameters$plans) {
      message("Building Primary ", p, " network")
      rp = uptake_list_school[[p]]
      rp = rp[rp$schooltype == "primary",]
      rnet = make_rnets(rp, ncores = 1)
      
      f = paste0("outputdata/rnet_primary_school_", p, ".Rds")
      saveRDS(rnet, f)
      rnet_primary_list[[p]] = rnet
    }
    
    #Secondary
    rnet_secondary_list = sapply(parameters$plans, function(x) NULL)
    for(p in parameters$plans) {
      message("Building Secondary ", p, " network")
      rs = uptake_list_school[[p]]
      rs = rs[rs$schooltype == "secondary",]
      rnet = make_rnets(rs, ncores = 1)
      f = paste0("outputdata/rnet_secondary_school_", p, ".Rds")
      saveRDS(rnet, f)
      rnet_secondary_list[[p]] = rnet
    }
    
    rnet_school_list =list(rnet_primary_list, rnet_secondary_list)
    names(rnet_school_list) = c("Primary","Secondary")
    
    saveRDS(rnet_school_list, "outputdata/rnet_school_list.Rds")
    rnet_school_list
  }),
  
  tar_target(combined_network, {
    
    # Purpose: Combine indervidual rnets into single rnet -----------------------
    # If stored locally:
    # rnet_commute_list = readRDS("outputdata/rnet_commute_list.Rds")
    # rnet_school_list = readRDS("outputdata/rnet_school_list.Rds")
    rnet_cl = rnet_commute_list
    rnet_sl_p = rnet_school_list$Primary
    rnet_sl_s = rnet_school_list$Secondary
    names(rnet_cl) = paste0("commute_", names(rnet_cl))
    names(rnet_sl_p) = paste0("primary_", names(rnet_sl_p))
    names(rnet_sl_s) = paste0("secondary_", names(rnet_sl_s))
    
    rnet_combined = combine_rnets(rnl = c(rnet_cl, rnet_sl_p, rnet_sl_s),
                                  ncores = 1, 
                                  regionalise = 1e5,
                                  add_all = TRUE)
    # Sort rnet for tileing, low values drawn first
    rnet_combined = rnet_combined[order(rnet_combined$all_fastest_bicycle_go_dutch, 
                                        rnet_combined$all_quietest_bicycle_go_dutch),]
    
    rnet_combined
  }),
  
  tar_target(combined_network_tile, {
    # Not All Data is put into the UI
    rnet_tile = combined_network
    
    # Remove Balanced Network
    nms = !grepl("(balanced)",names(rnet_tile))
    rnet_tile = rnet_tile[,nms]
    
    # Only use ebike routing for ebike scenario
    nms_noebike = !grepl("(ebike)",names(rnet_tile)) #keep all non-ebike
    nms_ebike2 = grepl("ebike.*ebike",names(rnet_tile)) # keep ebike twice
    nms_ebike1 = grepl("ebike",names(rnet_tile)) # keep quietest ebike
    nms_ebike1 = nms_ebike1 & !nms_ebike2
    nms_ebike1 = nms_ebike1 & grepl("quietest",names(rnet_tile))
    
    rnet_tile = rnet_tile[,nms_noebike | nms_ebike2 | nms_ebike1]
    names(rnet_tile) = gsub("_ebike_","_fastest_",names(rnet_tile))
    
    #Order Variaibles
    nms_end = c("Gradient","Quietness","geometry" )
    nms = names(rnet_tile)[!names(rnet_tile) %in% nms_end]
    rnet_tile = rnet_tile[c(nms[order(nms)], nms_end)]
    
    rnet_tile
  }),
  
  # tar_target(calculate_benefits, {
  #   TODO
  # }),
  
  # TODO: re-write zone stats code without needing huge uptake_list_commute object
  tar_target(zones_stats_list, {
    length(rnet_commute_list) # Ensure it runs after this
    # Create list object without the huge geometries:
    # Drop Geometry
    comm <- lapply(parameters$plans, function(plan) {
      sf::st_drop_geometry(readRDS(paste0("outputdata/uptake_commute_", plan, ".Rds")))
    })
    
    # Summarise results by DataZone and School
    zones_stats_list = uptake_to_zone_stats(comm, uptake_list_school, zones)
    zones_stats_list
  }),
  # 
  # tar_target(zones_stats, {
  #   zones_stats_list$zones
  # }),
  # 
  # tar_target(school_stats, {
  #   zones_stats_list$schools
  # }),
  # 
  # tar_target(zones_stats_json, {
  #   export_zone_json(zones_stats, "DataZone", path = "outputdata")
  # }),
  # 
  # tar_target(school_stats_json, {
  #   export_zone_json(school_stats, "SeedCode", path = "outputdata")
  # }),
  
  
  tar_target(save_outputs, {
    message("Saving outputs for ", parameters$date_routing)
    saveRDS(rnet_commute_list, "outputdata/rnet_commute_list.Rds")
    saveRDS(od_commute_subset, "outputdata/od_commute_subset.Rds")
    
    # TODO: re-implement:
    # saveRDS(zones_stats, "outputdata/zones_stats.Rds")
    # saveRDS(school_stats, "outputdata/school_stats.Rds")
    
    sys_time = Sys.time()
    # See code in R/make_geojson.R
    make_geojson_zones(combined_network_tile, "outputdata/combined_network_tile.geojson")
    # Tile the data:
    # system("bash code/tile.sh")
    # # Manually get geojson:
    # cd outputdata
    # gh release download z2023-07-28 --pattern *.geojson
    # cd ..
    if(!file.exists("outputdata/combined_network_tile.geojson")) {
      stop("No combined network")
    } 
    msg_verbose = paste0(
      "--name=rnet --layer=rnet --attribution=UniverstyofLeeds --minimum-zoom=6 ",
      "--maximum-zoom=13 --drop-smallest-as-needed --maximum-tile-bytes=5000000 ",
      "--simplification=10 --buffer=5 --force  outputdata/combined_network_tile.geojson"
    )
    date_routing = parameters$date_routing
    msg = glue::glue("tippecanoe -o outputdata/rnet_{date_routing}.pmtiles")
    system(paste(msg, msg_verbose))
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
    
    length(combined_network)
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
    metadata_all = tar_meta()
    metadata_targets = metadata_all %>% 
      filter(type == "stem")
    readr::write_csv(metadata_targets, "outputs/metadata_targets.csv")
    
    # TODO: add more columns
    build_summary = tibble::tibble(
      n_segment_cells = nrow(combined_network) * ncol(combined_network),
      min_flow = parameters$min_flow,
      max_to_route = parameters$max_to_route,
      time_total_mins = round(sum(metadata_targets$seconds) / 60, digits = 2),
      time_r_commute_mins = round(metadata_targets %>% 
                                    filter(name == "r_commute") %>% 
                                    pull(seconds) / 60, 
                                  digits = 2),
      routing_date = get_routing_date()
    )
    # # To overwrite previous build summary:
    # write_csv(build_summary, "outputs/build_summary.csv")
    if (file.exists("outputs/build_summary.csv")) {
      build_summary_previous = read_csv("outputs/build_summary.csv")
    } else {
      build_summary_previous = NULL
    }
    # Combine previous and current build datasets
    build_summary = data.table::rbindlist(list(build_summary, build_summary_previous), fill = TRUE)
    write_csv(build_summary, "outputs/build_summary.csv")
  })
)

# # Download a snapshot of the data:
# setwd("outputdata")
# system("gh release download v2023-03-24-22-28-51_commit_e2a60d0f06e6ddbf768382b19dc524cb3824c0c4 ")
