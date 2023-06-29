# Prepare Hands up Scotland data

library(sf)
library(tmap)
library(stplanr)
library(dplyr)
tmap_mode("view")

secure_path = Sys.getenv("NPT_TEAMS_PATH")

fls = list.files(file.path(secure_path,"secure_data/schools/raw_data/hands_up_scotland/"), full.names = TRUE)

res <- list()
for(i in 1:length(fls)){
  
  sub_nursery = readxl::read_xlsx(fls[i], sheet = "Table 3.1", range = "B10:N1000")
  sub_primary = readxl::read_xlsx(fls[i], sheet = "Table 3.2", range = "B10:N1000")
  sub_secondary = readxl::read_xlsx(fls[i], sheet = "Table 3.3", range = "B10:N1000")
  sub_sen = readxl::read_xlsx(fls[i], sheet = "Table 3.4", range = "B10:N1000")
  sub_independent = readxl::read_xlsx(fls[i], sheet = "Table 3.5", range = "B10:N1000")
  
  sub_nursery     = sub_nursery[!is.na(sub_nursery$Total),]
  sub_primary     = sub_primary[!is.na(sub_primary$Total),]
  sub_secondary   = sub_secondary[!is.na(sub_secondary$Total),]
  sub_sen         = sub_sen[!is.na(sub_sen$Total),]
  sub_independent = sub_independent[!is.na(sub_independent$Total),]
  
  sub_nursery$type <- "nursery"
  sub_primary$type <- "primary"
  sub_secondary$type <- "secondary"
  sub_sen$type <- "sen"
  sub_independent$type <- "independent"
  
  names(sub_nursery)[2] <- "SEED No." # Nursery's have a different ID system
  
  sub_all = rbind(sub_nursery, sub_primary, sub_secondary, sub_sen, sub_independent)
  
  res[[i]] = sub_all
  
}

res_all = bind_rows(res)

# West Lothian current missing and may need special treatment

# Clean
res_all[4:13] <- lapply(res_all[4:13], function(x){round(as.numeric(x),2)})

saveRDS(res_all,file.path(secure_path,"secure_data/schools/hands_up_scotland.Rds"))



