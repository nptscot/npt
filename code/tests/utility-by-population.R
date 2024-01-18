library(tmap)
tmap_mode("view")
tmap_options(check.and.fix = TRUE)
library(sf)
library(targets)
library(tidyverse)
library(ggplot2)

routes_fast = readRDS("./outputdata/2024-01-17/routes_utility_fastest.Rds")
tar_load(intermediate_zones)
interzone = intermediate_zones %>% 
  st_drop_geometry()

fast_origin = routes_fast %>% 
  st_drop_geometry() %>% 
  group_by(geo_code1) %>% 
  summarise(all = sum(all))
routes_join = inner_join(intermediate_zones, fast_origin, by = c("InterZone" = "geo_code1"))
summary(routes_join$all)


shopping_origin = routes_fast %>% 
  st_drop_geometry() %>% 
  filter(purpose == "shopping") %>% 
  group_by(geo_code1) %>% 
  summarise(all = sum(all))
routes_shopping = inner_join(intermediate_zones, shopping_origin, by = c("InterZone" = "geo_code1"))

visiting_origin = routes_fast %>% 
  st_drop_geometry() %>% 
  filter(purpose == "visiting") %>% 
  group_by(geo_code1) %>% 
  summarise(all = sum(all))
routes_visiting = inner_join(intermediate_zones, visiting_origin, by = c("InterZone" = "geo_code1"))

leisure_origin = routes_fast %>% 
  st_drop_geometry() %>% 
  filter(purpose == "leisure") %>% 
  group_by(geo_code1) %>% 
  summarise(all = sum(all))
routes_leisure = inner_join(intermediate_zones, leisure_origin, by = c("InterZone" = "geo_code1"))

tm_shape(routes_join) + tm_polygons()


ggplot(routes_join, aes(ResPop2011, all)) + 
  geom_point()

ggplot(routes_shopping, aes(ResPop2011, all)) + 
  geom_point() +
  ylab("Shopping (all modes)")
ggplot(routes_visiting, aes(ResPop2011, all)) + 
  geom_point() +
  ylab("Visiting (all modes)")
ggplot(routes_leisure, aes(ResPop2011, all)) + 
  geom_point() +
  ylab("Leisure (all modes)")

routes_join = routes_join %>% 
  mutate(ratio = all / ResPop2011)
summary(routes_join$ratio)

few_trips = routes_join %>% 
  filter(ratio < 0.75)

tm_shape(routes_join) + tm_polygons("ratio") 
