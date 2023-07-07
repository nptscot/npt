library(targets)
library(tidyverse)
tar_source()
tar_load(r_school)
plot(r_school$fastest$geometry)
tar_load(parameters)
print(parameters)

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