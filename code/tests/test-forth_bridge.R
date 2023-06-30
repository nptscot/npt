library(sf)
library(mapedit)
library(tmap)

desire_lines = read_rds("desire_lines_scotland.Rds")


tmap_mode("view")


tm_shape(desire_lines)+
  tm_lines()


line = mapedit::editMap()

tm_shape(line)+
  tm_lines()


desire_lines_subset = desire_lines[line, ]

sum(desire_lines_subset$bicycle)
desire_lines_subset |>
  filter(dist_euclidean < 2e4) |>
  tm_shape() +
  tm_lines(col = "bicycle")



desire_lines_subset |> filter(dist_euclidean < 2e4) |> pull(bicycle) |> sum()
