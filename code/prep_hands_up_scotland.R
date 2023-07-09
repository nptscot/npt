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

saveRDS(res_all,file.path(secure_path,"secure_data/schools/hands_up_scotland.Rds"))

# West Lothian current missing and may need special treatment

# Clean
res_all[4:13] <- lapply(res_all[4:13], function(x){round(as.numeric(x),2)})

# Fill in missing data
 res_summary <- res_all %>%
   group_by(`Local Authority`, `type`) %>%
   summarise(walk = median(Walk, na.rm = TRUE),
             bicycle = median(Cycle, na.rm = TRUE),
             scooter = median(`Scooter / Skate`, na.rm = TRUE),
             park_stride = median(`Park & Stride`, na.rm = TRUE),
             car = median(Driven, na.rm = TRUE),
             bus = median(Bus, na.rm = TRUE),
             taxi = median(Taxi, na.rm = TRUE),
             other = median(Other, na.rm = TRUE))

res_summary$total_p <- rowSums(res_summary[3:10])
summary(res_summary$total_p)

names(res_all) = c("la","SEED","school_name","year","walk","bicycle","Scooter / Skate",
                   "Park & Stride","Driven","Bus","Taxi","Other","total_pupils","type"  )

res_all$other <- rowSums(res_all[,c("Scooter / Skate","Park & Stride","Driven","Bus","Taxi","Other")], na.rm = TRUE)
res_all = res_all[,c("la","SEED","school_name","walk","bicycle","other","total_pupils","type")]

res_all$other <- ifelse(is.na(res_all$total_pupils),NA,res_all$other)

res_summary = res_summary[,c("Local Authority","type","walk","bicycle")]
names(res_summary) = c("la","type","walk_ave","bicycle_ave")

res_join = left_join(res_all, res_summary, by = c("la","type"))

res_join$walk = ifelse(is.na(res_join$walk),res_join$walk_ave,res_join$walk)
res_join$bicycle = ifelse(is.na(res_join$bicycle),res_join$bicycle_ave,res_join$bicycle)
res_join$other = ifelse(is.na(res_join$other),1 - res_join$bicycle - res_join$walk,res_join$other)
res_join$data = ifelse(is.na(res_join$total_pupils),"modeled","observed")

#TODO: Better method of filling in gaps
res_join$bicycle = ifelse(is.na(res_join$bicycle),median(res_join$bicycle, na.rm = TRUE),res_join$bicycle)
res_join$walk = ifelse(is.na(res_join$walk),median(res_join$walk, na.rm = TRUE),res_join$walk)

# Check other matches 
#res_join$foo = res_join$other -  (1 - res_join$bicycle - res_join$walk)
res_join$other = 1 - res_join$bicycle - res_join$walk

# Some SEED codes duplicated when two schools on the same site. Sort to favor real data
res_join <- res_join[order(res_join$data, decreasing = TRUE),]
res_join <- res_join[!duplicated(res_join$SEED),]

saveRDS(res_join,file.path(secure_path,"secure_data/schools/school_mode_split.Rds"))
