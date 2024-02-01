library(tmap)
tmap_mode("view")
library(targets)
library(tidyverse)

routes_utility_fast = readRDS("./outputdata/2024-01-17/routes_utility_fastest.Rds")

summary(routes_utility_fast)
# tm_shape(routes_utility_fast) + tm_lines()

s_area = sf::read_sf("data-raw/study_area.geojson")
routes_geosubset = routes_utility_fast[s_area, op = sf::st_crosses]

summary(routes_geosubset$distances) # Up to 23km!
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    765    3804    4816    4813    5771   23977 
summary(routes_geosubset$dist_euclidean_jittered) # between 500m to 5km
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  631.4  2810.4  3583.8  3481.6  4303.3  4997.5 
# tm_shape(routes_geosubset) + tm_lines()

weighted.mean(routes_geosubset$distances, w = routes_geosubset$bicycle)
# [1] 4453.714
weighted.mean(routes_geosubset$dist_euclidean_jittered, w = routes_geosubset$bicycle)
# [1] 3140.049

routes_geosubset = routes_geosubset |>
    mutate(distance_km = distances/1000)
library(ggplot2)
g1 = ggplot(routes_geosubset, aes((distance_km), y = ..density.., weight = bicycle)) + 
geom_histogram() +
labs(y = "Density", x = "Route distance (km) for utility trips")
g2 = g1 + theme(axis.title = element_text(size = 16),
axis.text = element_text(size = 16))
ggsave("./outputdata/distances-hist.png", g2)

g3 = ggplot(routes_geosubset, aes(dist_euclidean_jittered/1000, y = ..density.., weight = bicycle)) + 
geom_histogram() +
labs(y = "Density", x = "Euclidean distance (km) for utility trips")
g4 = g3 + theme(axis.title = element_text(size = 16),
axis.text = element_text(size = 16))
ggsave("./outputdata/euclidean-hist.png", g4)
# We need to get rid of long routes (e.g. over 8km) 
