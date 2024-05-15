## code to prepare `od_jittered_demo` dataset goes here

usethis::use_data(od_jittered_demo, overwrite = TRUE)
targets::tar_load(od_jittered)
set.seed(2022)
od_jittered_demo = od_jittered |>
  filter(all > 18)
od_jittered_demo = od_jittered[sample(nrow(od_jittered_demo), size = 100), ]
usethis::use_data(od_jittered_demo)
sf::write_sf(od_jittered_demo, "od_jittered_demo.geojson")
gh_release_upload("od_jittered_demo.geojson")
