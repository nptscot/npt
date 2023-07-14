# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
remotes::install_github("cyclestreets/cyclestreets-r", ref = "69-speed-up-json2sf_cs")
remotes::install_github("dabreegster/odjitter", subdir = "r")
library(targets)
library(tidyverse)
# library(tmap)
library(stplanr)
library(sf)

# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
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
      s_area = get_area("Forth Bridge", d = 20)
    } else {
      s_area = NULL
    }
    s_area
  }),
  # tar_target(dl_data, {
  #   setwd("inputdata")
  #   system("gh release download v1")
  #   setwd("..")
  # }),
  tar_target(zones, {
    z = readRDS("inputdata/zones_national_simple.Rds") # 1230 zones
    if(parameters$geo_subset) {
      z = z[study_area, op = sf::st_within]
    }
    z
  }),
  # To get the raw data:
  # tar_target(od_commute_raw, {
  #   # read_csv("data-raw/od_subset.csv")
  #   # See data-raw-get_wpz.R
  #   readRDS("inputdata/od_izo.Rds")
  # }),
  tar_target(od_data, {
    min_flow = parameters$min_flow # Set to 1 for full build, set to high value (e.g. 400) for tests
    desire_lines_raw = readRDS("inputdata/desire_lines_scotland.Rds")
    od_raw = as_tibble(sf::st_drop_geometry(desire_lines_raw))
    od_subset = od_raw %>%
      filter(geo_code1 %in% zones$InterZone) %>%
      filter(geo_code2 %in% zones$InterZone) %>%
      filter(dist_euclidean < 20000) %>%
      filter(dist_euclidean > 1000) %>%
      filter(all >= min_flow)
    od_subset
  }),
  tar_target(subpoints_origins, {
    # source("data-raw/get_wpz.R")
    # sf::read_sf("data-raw/oas.geojson")
    readRDS("inputdata/oas.Rds")
  }),
  tar_target(subpoints_destinations, {
    # source("data-raw/get_wpz.R")
    # sf::read_sf("data-raw/workplaces_simple_edinburgh.geojson")
    readRDS("inputdata/workplaces_simple.Rds")
  }),
  tar_target(od_commute_jittered, {
    # od_jittered = od_data # for no jittering
    # Install the Rust crate and the associated R package:
    # system("cargo install --git https://github.com/dabreegster/odjitter")
    
    set.seed(2023)
    odj = odjitter::jitter(
      od = od_data,
      zones = zones,
      subpoints_origins = subpoints_origins,
      subpoints_destinations = subpoints_destinations,
      disaggregation_threshold = 30
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
      filter(dist_euclidean_jittered < 20000) %>%
      filter(dist_euclidean_jittered > 500) %>%
      slice_max(n = parameters$max_to_route, order_by = bicycle, with_ties = FALSE)
    odcs
  }),
  
  tar_target(r_commute, {
    
    message(parameters$date_routing)
    message("Calculating ", nrow(od_commute_subset), " routes")
    # Test routing:
    # stplanr::route(l = od_to_route, route_fun = cyclestreets::journey, plan = "balanced")
    folder_name = paste0("outputdata/", parameters$date_routing)
    
    routes_commute = get_routes(od = od_commute_subset,
                                plans = parameters$plans, purpose = "commute",
                                folder = folder_name, batch = TRUE, nrow_batch = 99999,
                                batch_save = TRUE)
    routes_commute
  }),
  
  tar_target(r_school, {
    # Get School OD
    path_teams = Sys.getenv("NPT_TEAMS_PATH")
    if(nchar(path_teams) == 0){
      stop("Can't find Teams folder of secure data. Use usethis::edit_r_environ() to define NPT_TEAMS_PATH ")
    }
    if(file.exists(file.path(path_teams,"secure_data/schools/school_dl_sub30km.Rds"))){
      schools_dl = readRDS(file.path(path_teams, "secure_data/schools/school_dl_sub30km.Rds"))
    } else {
      # stop("Can't find ",file.path(path_teams,"secure_data/schools/school_dl_sub30km.Rds"))
      schools_dl = NULL
    }
    
    if(parameters$geo_subset) {
      schools_dl = schools_dl[study_area, op = sf::st_within]
    }
    schools_dl = schools_dl %>%
      slice_max(order_by = count, n = parameters$max_to_route, with_ties = FALSE)
    folder_name = paste0("outputdata/", parameters$date_routing)
    routes_school = get_routes(
        schools_dl,
        plans = parameters$plans, purpose = "school",
        folder = folder_name,
        batch = FALSE,
        nrow_batch = 100000
        )
      routes_school
  }),
  
  tar_target(uptake_list_commute, {
    p = "fastest"
    for(p in parameters$plans) {
      
      # # For local routes:
      # f = paste0("outputdata/routes_max_dist_commute_", p, ".Rds")
      # routes = readRDS(f)
      
      # # For routes from targets
      routes = r_commute[[p]]
      message("Uptake for ", p)
      # system.time({
      routes = routes %>%
        get_scenario_go_dutch() %>%
        as_tibble()
      routes[["geometry"]] = st_sfc(routes[["geometry"]], recompute_bbox = TRUE)
      routes = st_as_sf(routes)
      # })
      f = paste0("outputdata/routes_commute_", p, ".Rds")
      saveRDS(routes, f)
    }
    uptake_list_commute = lapply(parameters$plan, function(p) {
      f = paste0("outputdata/routes_commute_", p, ".Rds")
      readRDS(f)
    })
    names(uptake_list_commute) = parameters$plans
    uptake_list_commute
  }),
  
  tar_target(uptake_list_school, {
    p = "balanced"
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
  
  tar_target(rnet_commute_list, {
    
    rnet_commute_list = sapply(parameters$plans, function(x) NULL)
    #p = "fastest"
    for(p in parameters$plans) {
      message("Building ", p, " network")
      rnet = make_rnets(uptake_list_commute[[p]], ncores = 1)
      
      f = paste0("outputdata/rnet_commute_", p, ".Rds")
      # saveRDS(rnet, f)
      rnet_commute_list[[p]] = rnet
    }
    
    # saveRDS(rnet_commute_list, "outputdata/rnet_commute_list.Rds")
    rnet_commute_list
    
  }),
  
  tar_target(rnet_school_list, {
    
    rnet_school_list = sapply(parameters$plans, function(x) NULL)
    #p = "fastest"
    for(p in parameters$plans) {
      message("Building ", p, " network")
      rnet = make_rnets(uptake_list_school[[p]], ncores = 1)
      
      f = paste0("outputdata/rnet_school_", p, ".Rds")
      saveRDS(rnet, f)
      rnet_school_list[[p]] = rnet
    }
    
    saveRDS(rnet_school_list, "outputdata/rnet_school_list.Rds")
    rnet_school_list
  }),
  
  tar_target(combined_network, {
    
    # Purpose: Combine indervidual rnets into single rnet -----------------------
    # If stored locally:
    # rnet_commute_list = readRDS("outputdata/rnet_commute_list.Rds")
    # rnet_school_list = readRDS("outputdata/rnet_school_list.Rds")
    rnet_cl = rnet_commute_list
    rnet_sl = rnet_school_list
    names(rnet_cl) = paste0("commute_", names(rnet_cl))
    names(rnet_sl) = paste0("school_", names(rnet_sl))
    
    rnet_combined = combine_rnets(c(rnet_cl, rnet_sl),
                                  ncores = 1, 
                                  regionalise = 1e5,
                                  add_all = TRUE)
    # Sort rnet for tileing, low values drawn first
    rnet_combined = rnet_combined[order(rnet_combined$all_fastest_bicycle_go_dutch, 
                                        rnet_combined$all_quietest_bicycle_go_dutch),]
    
    rnet_combined
  }),
  
  tar_target(calculate_benefits, {
    benefits = function(x) x
    benefits(r_commute)
  }),
  
  tar_target(zone_stats, {
    r_commute
    zones
  }),
  
  tar_target(save_outputs, {
    message("Saving outputs for ", parameters$date_routing)
    saveRDS(rnet_commute_list, "outputdata/rnet_commute_list.Rds")
    saveRDS(od_commute_subset, "outputdata/od_commute_subset.Rds")
    saveRDS(combined_network, "outputdata/combined_network.Rds")
    # Saved by get_routes()
    # f = paste0("outputdata/routes_commute_", nrow(od_commute_subset), "_rows.Rds")
    # saveRDS(r_commute, f)
    sys_time = Sys.time()
    # # Bash code to get combined network before running the tiling code:
    # cd outputdata
    # gh release list
    # # See here for releases: https://github.com/nptscot/outputdata/releases
    # gh release download v2023-07-08-14-42-06.466773_commit_21ee5f538b7e3e13d60c59f9e2745858ca15c188 --pattern 'combined*' --clobber
    # combined_network = readRDS("outputdata/combined_network.Rds")
    make_geojson_zones(combined_network, "outputdata/combined_network.geojson")
    zip(zipfile = "outputdata/combined_network.zip", "outputdata/combined_network.geojson")
    file.rename("outputdata/combined_network.geojson", "rnet.geojson")
    # zip(zipfile = "outputdata/combined_network.zip", "rnet.geojson")
    # Tile the data:
    # system("bash code/tile.sh")
    msg_verbose = paste0(
      "--name=rnet --layer=rnet --attribution=UniverstyofLeeds --minimum-zoom=6 ",
      "--maximum-zoom=13 --drop-smallest-as-needed --maximum-tile-bytes=5000000 ",
      "--simplification=100 --buffer=5 --force --coalesce --visvalingam rnet.geojson"
    )
    # date_routing = "2023-07-14"
    date_routing = parameters$date_routing
    msg = glue::glue("tippecanoe -o outputdata/rnet_{date_routing}.pmtiles")
    system(paste(msg, msg_verbose))
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
    length(r_commute)
    commit = gert::git_log(max = 1)
    message("Commit: ", commit)
    
    if(Sys.info()[['sysname']] == "Linux" | TRUE ) {
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
    message("gh command line tool not available")
    message("Now create a release with this version number and upload the files")
  }
  Sys.Date()
  }),
  
  tar_target(metadata, {
    upload_data
    metadata_all = tar_meta()
    metadata_targets = metadata_all %>% 
      filter(type == "stem")
    readr::write_csv(metadata_targets, "outputs/metadata_targets.csv")
    
    # Todo: add more columns
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
