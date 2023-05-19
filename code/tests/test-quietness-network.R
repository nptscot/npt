# Aim: debug issue 125

remotes::install_cran("zonebuilder")

dundee_buffer_3km = zonebuilder::zb_zone(x = "Dundee", n_circles = 2)
mapview::mapview(dundee_buffer_3km)

# Load per plan network

library(targets)
tar_load(rnet_commute_list)
rnet_quietest_dundee = rnet_commute_list$quietest[dundee_buffer_3km, ]
mapview::mapview(rnet_quietest_dundee, zcol = "bicycle_go_dutch")

# Take code from targets:


rcl = rnet_commute_list
head(rcl[[1]])

names(rcl$fastest)[1:4] = paste0("fastest_", names(rcl$fastest)[1:4])
names(rcl$balanced)[1:4] = paste0("balanced_", names(rcl$balanced)[1:4])
names(rcl$quietest)[1:4] = paste0("quietest_", names(rcl$quietest)[1:4])
names(rcl$ebike)[1:4] = paste0("ebike_", names(rcl$ebike)[1:4])

names_combined = lapply(rcl, names) %>% unlist(use.names = FALSE)
names_combined = names_combined[names_combined != "geometry"]

# Saved lots of lines of code and faster:
rnet_long = data.table::rbindlist(rcl, fill = TRUE)
rnet_long = rnet_long %>% 
  mutate(across(fastest_bicycle:ebike_Quietness, function(x) tidyr::replace_na(x, 0))) %>% 
  as_tibble()

rnet_long$geometry = sf::st_sfc(rnet_long$geometry, recompute_bbox = TRUE)
rnet_long = sf::st_as_sf(rnet_long)
sf::st_geometry(rnet_long)

rnet_combined = overline(rnet_long, attrib = names_combined)
saveRDS(rnet_combined, "outputdata/rnet_combined_after_overline.Rds")
# # Testing outputs
# rnet_combined = readRDS("outputdata/rnet_combined_after_overline.Rds")
rnet_combined = rnet_combined %>% 
  rowwise() %>% 
  mutate(Gradient = max(fastest_Gradient, balanced_Gradient, quietest_Gradient, ebike_Gradient)) %>% 
  mutate(Quietness = max(fastest_Quietness, balanced_Quietness, quietest_Quietness, ebike_Quietness)) 

rnet = rnet_combined %>% 
  select(-matches("_Q|_Gr")) %>% 
  mutate(across(matches("bicycle", round))) %>% 
  mutate(Gradient = round(Gradient, digits = 1))
# # TODO: check gradients
# table(rnet_combined$Gradient)

rnet = rnet %>%
  rowwise() %>%
  mutate(total_cyclists = sum(fastest_bicycle:ebike_bicycle_go_dutch))
summary(rnet$total_cyclists)
names(rnet)[1:8] = paste0("commute_", names(rnet))[1:8]
rnet = rnet %>% 
  filter(total_cyclists > 0) %>% 
  select(-total_cyclists) %>% 
  as.data.frame() %>% 
  sf::st_as_sf()