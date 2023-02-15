

# Ok, so the data.table approach is something else (as expected I guess):
library(sf)  
library(tidyverse)
nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
set.seed(1234)
nc_list_10 <- nc %>%
  dplyr::sample_n(size = 10, replace = TRUE) %>%
  mutate(sample_id = paste0("sample_", row_number())) %>%
  split(.$sample_id)

bench::mark(max_iterations = 2, check = FALSE,
  rbind = do.call(what = rbind, args = nc_list_10),
  bind_rows = do.call(what = bind_rows, args = nc_list_10),
  rbindlist = sf::st_as_sf(data.table::rbindlist(nc_list_10)) # incorrect bb
)

waldo::compare(sf::st_as_sf(data.table::rbindlist(nc_list_10)), do.call(what = rbind, args = nc_list_10))

microbenchmark(
  mapedit:::combine_list_of_sf(nc_list_1k),
  do.call(what = sf:::rbind.sf,
          args = nc_list_1k),
  purrr::reduce(.x = nc_list_1k,
                .f = sf:::rbind.sf),
  sf::st_as_sf(data.table::rbindlist(nc_list_1k)),
  times = 25L
)
mapedit:::combine_list_of_sf(nc_list_1k)
do.call(what = sf:::rbind.sf, args = nc_list_1k)
purrr::reduce(.x = nc_list_1k, .f = sf:::rbind.sf)
sf::st_as_sf(data.table::rbindlist(nc_list_1k))

set.seed(1234)
nc_list_10k <- nc %>%
  dplyr::sample_n(size = 10000, replace = TRUE) %>%
  mutate(sample_id = paste0("sample_", row_number())) %>%
  split(.$sample_id)
microbenchmark(
  mapedit:::combine_list_of_sf(nc_list_10k),
  do.call(what = sf:::rbind.sf,
          args = nc_list_10k),
  sf::st_as_sf(data.table::rbindlist(nc_list_10k)),
  times = 25L
)
