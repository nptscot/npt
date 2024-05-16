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
# TODO: Two other LAs missing

# Clean
res_all[4:13] <- lapply(res_all[4:13], function(x){round(as.numeric(x),2)})

# Reduce modes to standard list https://github.com/nptscot/npt/issues/191
res_all$bicycle = res_all$Cycle
res_all$public_transport = res_all$Bus
res_all$taxi = res_all$Taxi
res_all$other = res_all$Other

# For multiple modes 0 + NA == NA but 0.5 + NA == 0.5
# This because 0 are provided but low values are suppressed
res_all$walk = ifelse(rowSums(res_all[,c("Walk","Scooter / Skate")], na.rm = TRUE) == 0,
                      NA,
                      rowSums(res_all[,c("Walk","Scooter / Skate")], na.rm = TRUE))
res_all$car = ifelse(rowSums(res_all[,c("Driven","Park & Stride")], na.rm = TRUE) == 0,
                      NA,
                      rowSums(res_all[,c("Driven","Park & Stride")], na.rm = TRUE)) 

res_all = res_all[,c("Local Authority","SEED No.","School Name","Year",
                     "Total","type","walk","bicycle","public_transport","car",
                     "taxi","other")]

# Fill in missing data
res_summary <- res_all |>
 filter(!is.na(Total)) |>
 group_by(`Local Authority`, `type`) |>
 summarise(walk = median(walk, na.rm = TRUE),
           bicycle = median(bicycle, na.rm = TRUE),
           public_transport = median(public_transport, na.rm = TRUE),
           car = median(car, na.rm = TRUE),
           taxi = median(taxi, na.rm = TRUE),
           other = median(other, na.rm = TRUE))

res_summary$total_p <- rowSums(res_summary[3:8])
summary(res_summary$total_p)
res_summary$total_p <- NULL

names(res_all) = c("la","SEED","school_name","year","total_pupils","type",
                   "walk","bicycle","public_transport","car","taxi","other")

res_all$other <- ifelse(is.na(res_all$total_pupils),NA,res_all$other)
names(res_summary) = c("la","type","walk_ave","bicycle_ave","public_transport_ave","car_ave",
                       "taxi_ave","other_ave")

res_join = left_join(res_all, res_summary, by = c("la","type"))

# Drop Nursery, SEN, Independent for now
res_join = res_join[!res_join$type %in% c("nursery","independent","sen"), ]

infill_modalshare = function(x){
  
  x = as.data.frame(x)
  
  # Check missing data
  if(all(c(!is.na(x$walk), !is.na(x$bicycle), !is.na(x$public_transport), !is.na(x$car), !is.na(x$taxi), !is.na(x$other)))){
    x = x[,c("la","SEED","id","school_name","year","total_pupils","type","walk","bicycle","public_transport","car","taxi","other")]
    return(x)
  }
  
  knownshare = rowSums(x[,c("walk","bicycle","public_transport","car","taxi","other")], na.rm = TRUE)
  knownshare = ifelse(knownshare > 1, 1, knownshare)
  
  unknownshare = 1 - knownshare
  unknownshare = ifelse(unknownshare > 1, 1, unknownshare)
  
  weights = as.numeric(t(x[,c("walk_ave","bicycle_ave","public_transport_ave","car_ave","taxi_ave","other_ave")]))
  names(weights) = c("walk","bicycle","public_transport","car","taxi","other")
  weights[is.na(weights)] = 0
  
  # Give 0 weight to known variables
  for(i in seq_along(weights)){
    if(!is.na(x[names(weights)[i]])){
      weights[i] <- 0
    }
  }
  
  if(sum(weights, na.rm = TRUE) == 0){
    weights[weights == 0] = 1
    for(i in seq_along(weights)){
      if(!is.na(x[names(weights)[i]])){
        weights[i] <- 0
      }
    }
    
    #stop("Zero Weights applied ",x$SEED)
  }
  
  # Distribute unknown by weight
  weights = weights / sum(weights)
  weights = weights * unknownshare
  
  # Hand out new mode share
  for(i in seq_along(weights)){
    if(is.na(x[names(weights)[i]])){
      x[names(weights)[i]] = weights[i]
    }
  }
  
  x = x[,c("la","SEED","id","school_name","year","total_pupils","type","walk","bicycle","public_transport","car","taxi","other")]
  return(x)
  
}

res_join$id <- seq(1,nrow(res_join))
res_split <- dplyr::group_split(res_join, res_join$id)

res_split = pbapply::pblapply(res_split, infill_modalshare)
res_split = bind_rows(res_split)

res_split[8:13] <- lapply(res_split[8:13], round, digits = 3)
res_split$ptot <- rowSums(res_split[8:13])

res_split$other <- ifelse(res_split$ptot == 1, res_split$other, res_split$other + 1 - res_split$ptot)
res_split$other <- ifelse(res_split$other < 0, 0, res_split$other)

res_split$ptot = NULL

summary(res_split)

saveRDS(res_split,file.path(secure_path,"secure_data/schools/school_mode_split.Rds"))

# stop()
# 
# res_join$walk = ifelse(is.na(res_join$walk),res_join$walk_ave,res_join$walk)
# res_join$bicycle = ifelse(is.na(res_join$bicycle),res_join$bicycle_ave,res_join$bicycle)
# res_join$public_transport = ifelse(is.na(res_join$public_transport),res_join$public_transport_ave,res_join$public_transport)
# res_join$car = ifelse(is.na(res_join$car),res_join$car_ave,res_join$car)
# res_join$taxi = ifelse(is.na(res_join$taxi),res_join$taxi_ave,res_join$taxi)
# res_join$other = ifelse(is.na(res_join$other),round(1 - res_join$bicycle - res_join$walk - 
#                                                       res_join$public_transport - res_join$car - 
#                                                       res_join$taxi, 2),res_join$other)
# res_join$data = ifelse(is.na(res_join$total_pupils),"modeled","observed")
# 
# summary(res_join$other) # TODO: Some have -ve other mode share as knonw modes are high %
# 
# #TODO: Better method of filling in gaps
# res_join$bicycle = ifelse(is.na(res_join$bicycle),median(res_join$bicycle, na.rm = TRUE),res_join$bicycle)
# res_join$walk = ifelse(is.na(res_join$walk),median(res_join$walk, na.rm = TRUE),res_join$walk)
# 
# # Check other matches 
# #res_join$foo = res_join$other -  (1 - res_join$bicycle - res_join$walk)
# #res_join$other = 1 - res_join$bicycle - res_join$walk
# 
# # Some SEED codes duplicated when two schools on the same site. Sort to favor real data
# res_join <- res_join[order(res_join$data, decreasing = TRUE),]
# res_join <- res_join[!duplicated(res_join$SEED),]
# 
# res_join <- res_join[,c("la","SEED","school_name","year","data","total_pupils","type",
#                         "walk","bicycle","public_transport","car","taxi","other")]