# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint
library(tidyverse)
# library(tmap)
library(stplanr)
library(sf)
remotes::install_github("cyclestreets/cyclestreets-r")
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
# source("other_functions.R") # Source other scripts as needed. # nolint

# Build parameters --------------------------------------------------------

# # # Computation done outside of the pipeline --------------------------------
# #
# plans = c("fastest", "balanced", "quietest", "ebike")
# tar_load(od_commute_subset)
# i = plans[1]
# # for(i in plans) {
# #   cyclestreets::batch(desire_lines = od_commute_subset, username = "robinlovelace", strategies = i)
# # }
# routes_commute = get_routes(od_commute_subset,
#                     plans = plans, purpose = "commute",
#                     folder = "outputdata", batch = FALSE, nrow_batch = 12500)
# saveRDS(routes_commute, "outputdata/routes_commute.Rds")

# Targets -----------------------------------------------------------------

# Replace the target list below with your own:
list(
  tar_target(parameters, {
    renviron_exists = file.exists(".Renviron")
    if(!renviron_exists) {
      warning("No .Renviron file, routing may not work")
    }
    list(
      plans = c("fastest", "balanced", "quietest", "ebike"),
      # plans = c("fastest"),
      # min_flow = 300, # Set to 1 for full build, set to high value (e.g. 400) for tests
      min_flow = 1,
      # max_to_route = 29, # Set to 10e6 or similar large number for all routes
      max_to_route = 10e6,
      date_routing = "2023-02-16"
      )
  }),

  tar_target(dl_data, {
    setwd("inputdata")
    gh_release_downlad(tag = "v1")
    setwd("..")
  }),
  tar_target(zones,
    command = {

      # For Edinburgh data (test):
      # sf::read_sf("data-raw/zones_edinburgh.geojson")
      # For national data:
      readRDS("inputdata/zones_national_simple.Rds") # 1230 zones
    }),
  tar_target(od_commute_raw, {
    # read_csv("data-raw/od_subset.csv")
    readRDS("inputdata/od_izo.Rds")
  }),
  # tar_target(od_schools_raw, {
  #   # read_csv("data-raw/od_subset.csv")
  #   # read_csv("data-raw/od_subset.csv")
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
    # write_csv(od_subset, "data-raw/od_subset.csv")
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
    # od_jittered = od_data # for no jittering:
    remotes::install_github("dabreegster/odjitter", subdir = "r")
    set.seed(2023)
    odj = odjitter::jitter(
      od = od_data,
      zones = zones,
      subpoints_origins = subpoints_origins,
      subpoints_destinations = subpoints_destinations,
      disaggregation_threshold = 40
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
      filter(dist_euclidean < 20000) %>%
      filter(dist_euclidean > 500) %>%
      top_n(n = parameters$max_to_route, wt = bicycle)
    odcs
  }),
  tar_target(r_commute, {

    message("Calculating ", nrow(od_commute_subset), " routes")
    # Test routing:
    # stplanr::route(l = od_to_route, route_fun = cyclestreets::journey, plan = "balanced")

    # For all plans:
    routes = readRDS("outputdata/routes_commute.Rds")
    # routes = get_routes(od_commute_subset,
    #            plans = parameters$plans, purpose = "commute",
    #            folder = "outputdata", batch = FALSE, nrow_batch = 12500)
    class_routes = class(routes)
    if(any("sf" %in% class(routes))) {
      routes = list(fastest = routes)
    }
    plans = parameters$plans
    message("Getting these plans: ", paste0(plans, collapse = ", "))
    uptake_list = sapply(plans, function(x) NULL)
    for(p in parameters$plans) {
      uptake_list[[p]] = get_scenario_go_dutch(routes[[p]])
    }
    uptake_list
  }),
  tar_target(rnet_commute_list, {
    rnet_commute_list = sapply(parameters$plans, function(x) NULL)
    for(p in parameters$plans) {
      message("Building ", p, " network")
      rnet_raw = stplanr::overline(
        r_commute[[p]],
        attrib = c("bicycle", "bicycle_go_dutch", "quietness", "gradient_smooth"), # todo: add other modes
        fun = list(sum = sum, mean = mean)
      )
      rnet = rnet_raw %>%
        transmute(
          bicycle = round(bicycle_sum),
          # `Bicycle (Near Market)` = round(cyclists_near_sum),
          bicycle_go_dutch = round(bicycle_go_dutch_sum),
          # `Bicycle (Ebike)` = round(cyclists_ebike_sum),
          Gradient = round(gradient_smooth_mean * 100),
          Quietness = round(quietness_mean)
          # col = cut(Quietness, quietness_breaks, labels = pal_quietness, right = FALSE)
        ) %>%
        dplyr::arrange(bicycle)
      rnet_commute_list[[p]] = rnet
    }
    rnet_commute_list
  }),
  tar_target(combined_network, {
    rcl = rnet_commute_list

    names(rcl$fastest)[1:4] = paste0("fastest_", names(rcl$fastest)[1:4])
    names(rcl$balanced)[1:4] = paste0("balanced_", names(rcl$balanced)[1:4])
    names(rcl$quietest)[1:4] = paste0("quietest_", names(rcl$quietest)[1:4])
    names(rcl$ebike)[1:4] = paste0("ebike_", names(rcl$ebike)[1:4])
    
    names_combined = lapply(rcl, names) %>% unlist(use.names = FALSE)
    names_combined = names_combined[names_combined != "geometry"]
    
    # Saved lots of lines of code and faster:
    rnet_long = data.table::rbindlist(rcl, fill = TRUE)
    # names(rnet_long)
    rnet_long$geometry = sf::st_sfc(rnet_long$geometry, recompute_bbox = TRUE)
    rnet_long = sf::st_as_sf(rnet_long)
    rnet_long = rnet_long %>% 
      mutate(across(fastest_bicycle:ebike_Quietness, function(x) tidyr::replace_na(x, 0)))
    # summary(rnet_long)
    rnet_combined = overline(rnet_long, attrib = names_combined)
    rnet_combined = rnet_combined %>% 
      rowwise() %>% 
      mutate(Gradient = max(fastest_Gradient, balanced_Gradient, quietest_Gradient, ebike_Gradient)) %>% 
      mutate(Quietness = max(fastest_Quietness, balanced_Quietness, quietest_Quietness, ebike_Quietness)) 
    # summary(rnet_combined$Gradient)
    # summary(rnet_combined$fastest_Gradient)

    rnet = rnet_combined %>% 
      select(-matches("_Q|_Gr")) %>% 
      mutate(across(matches("bicycle", round))) %>% 
      mutate(Gradient = round(Gradient, digits = 1))
    # table(rnet_combined$quietest_bicycle_go_dutch)
    # # TODO: check gradients
    # table(rnet_combined$Gradient)

    rnet = rnet %>% 
      rowwise() %>% 
      mutate(total_cyclists = sum(fastest_bicycle:ebike_bicycle_go_dutch))
    summary(rnet$total_cyclists)
    rnet = rnet %>% 
      filter(total_cyclists > 0) %>% 
      select(-total_cyclists)

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
    saveRDS(rnet_commute_list, "outputdata/rnet_commute_list.Rds")
    saveRDS(od_commute_subset, "outputdata/od_commute_subset.Rds")
    saveRDS(combined_network, "outputdata/tar_combine_network.Rds")
    # Saved by get_routes()
    # f = paste0("outputdata/routes_commute_", nrow(od_commute_subset), "_rows.Rds")
    # saveRDS(r_commute, f)
    Sys.time()
  }),
  
  # tar_target(plot_zones, {
  #   # tm_shape(zones) +
  #   m = tm_shape(zones) +
  #     tm_fill(col = "TotPop2011", palette = "viridis")
  #   tmap_save(m, "figures/test-plot.png")
  # }),
  
  # tar_target(visualise_rnet, {
  #   # tar_source("code/vis_network.R")
  #   # tarchetypes::tar_
  # }),
  # tarchetypes::tar_render(visualise_rnet, path = "code/vis_network.Rmd", params = list(rnet_commute_list)),
  
  # tarchetypes::tar_render(report, path = "README.Rmd", params = list(zones, rnet)),
  tar_target(upload_data, {
    length(combined_network)
    length(r_commute)
    commit = gert::git_log(max = 1)
    v = paste0("v", save_outputs, "_commit_", commit$commit)
    v = gsub(pattern = " |:", replacement = "-", x = v)
    setwd("outputdata")
    f = list.files(path = ".", pattern = "Rds")
    # Piggyback fails with error message so commented and using cust
    # piggyback::pb_upload(f)
    msg = glue::glue("gh release create {v} --generate-notes")
    message("Creating new release and folder to save the files: ", v)
    dir.create(v)
    system(msg)
    for(i in f) {
      gh_release_upload(file = i, tag = v)
      # Move into a new directory
      file.copy(from = v, to = file.path(v, i))
    }
    message("Files stored in output folder: ", v)
    message("Which contains: ", paste0(list.files(v), collapse = ", "))
    # For rds based version:
    # For specific version:
    # system("gh release create v0.0.1 --generate-notes")
    file.remove(f)
    setwd("..")
  })
  # tar_source(files = "data-raw/test-tiles.R") # how to source script as target?
)
# Explore results for Edinburgh
# tar_load(rnet)
# ed = sf::read_sf("data-raw/zones_edinburgh.geojson")
# rnet_ed = rnet[ed, ]
# tmap_mode("view")
# tar_load(zones)
# tar_load(subpoints_origins)
# tar_load(subpoints_destinations)
#
# tm_shape(rnet_ed) +
#   tm_lines(lwd = "bicycle_go_dutch", scale = 19) +
#   tm_shape(ed) +
#   tm_borders(col = "red") +
#   tm_shape(subpoints_origins) +
#   tm_dots(col = "green") +
#   tm_shape(subpoints_destinations) +
#   tm_dots(col = "blue")
