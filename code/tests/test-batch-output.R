library(tidyverse)

ds = readr::read_csv("test-large-batch-dataset.csv.gz")
# Error:
# Removing NA routes: 8134 8748 12754 31108 38245 46070 54354 64707
# Error : lexical error: invalid char in json text.
# ionName":"Main road","type :"segment"}},{"@attributes":{"nam
#                      (right here) ------^
# 
# Starting routing
# POSTing the request to create and start the job
# Job id: 4667
ds_issue = ds %>%
  # head() %>% 
  filter(str_detect(json, pattern = '\\"type'))
jsonlite::fromJSON(ds$json[1])
RcppSimdJson::fparse(ds$json[1])
bench::mark(check = FALSE,
  jsonlite = jsonlite::fromJSON(ds$json[1]),
  simd = RcppSimdJson::fparse(ds$json[1])
)
ljson = lapply(ds$json, RcppSimdJson::fparse)
ljson[[1]]
