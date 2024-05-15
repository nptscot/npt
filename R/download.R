dl_nrs = function(
    base_url = "https://www.nrscotland.gov.uk/files//statistics/geography/",
    file = "workplaces-zones-2011-scotland-centroids.zip") {
  u = paste0(base_url, file)
  if (!file.exists(file)) {
    download.file(u, file)
  }
}
