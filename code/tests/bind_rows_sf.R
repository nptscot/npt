group_by

# Ok, so the data.table approach is something else (as expected I guess):
library(sf)  
library(tidyverse)
nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
set.seed(1234)
nc_list_10 <- nc |>
  dplyr::sample_n(size = 10, replace = TRUE) |>
  mutate(sample_id = paste0("sample_", row_number())) |>
  split(.$sample_id)

bench::mark(max_iterations = 2, check = FALSE,
  rbind = (res_rbind <<- do.call(what = rbind, args = nc_list_10)),
  dplyr::bind_rows = (res_bind_rows <<- do.call(what = bind_rows, args = nc_list_10)),
  rbindlist = (res_rbindlist <<- sf::st_as_sf(data.table::rbindlist(nc_list_10))), # incorrect bb
  res_unlist2d = (res_unlist2d <<- st_as_sf(collapse::unlist2d(nc_list_10, idcols = FALSE, recursive = FALSE)))
  # ,
  # unlist = {res_unlist2d <<- lapply(1:n, function(x) nc_list_10)
  # list2DF(lapply(setNames(seq_along(.[[1]]), names(.[[1]])), \(i)
  #                unlist(lapply(., `[[`, i), FALSE, FALSE)))}
)

waldo::compare(res_rbind, res_bind_rows)
waldo::compare(res_rbind, res_rbindlist) # bbox is wrong
waldo::compare(res_rbind, res_unlist2d) # bbox is wrong
waldo::compare(res_rbindlist, res_unlist2d) # bbox is wrong
