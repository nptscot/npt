# Wrapper function for making rnets
# r routes
# type character e.g. "school_fastest"
# ncores passed to overline
# regionalise passed to overline

#TODO: inconsident captialisation Quietness vs quietness

make_rnets = function(r, type, ncores = 20, regionalise = 1e5){
  r = r[r$bicycle_go_dutch > 0 | r$bicycle > 0,]
  rnet = stplanr::overline2(r, 
                            attrib = c("bicycle","bicycle_go_dutch","gradient","quietness"), 
                            fun = list(sum = sum, mean = mean),
                            ncores = ncores, regionalise = regionalise)
  rnet = rnet[,c("bicycle_sum","bicycle_go_dutch_sum","gradient_mean","quietness_mean")]
  rnet = rnet[rnet$bicycle_go_dutch > 0 , ]
  names(rnet) = c(paste0(type,"_bicycle"),paste0(type,"_bicycle_go_dutch"),"gradient","quietness","geometry")
  rnet
}

# Function to merge list of rnets into a single rnet
# rnl list of rnets

rnl = rnet_commute_list
names(rnl) = paste0("commute_", names(rnl))


# Takes a list of rnets and combines into a single rnet
# Names of the list are appended to the start of column names
# e.g. "commute_fastest_" is addeded to "bicycle_go_dutch"
# Gradient & Quietness are not renamed

combine_rnets = function(rnl, ncores = 20, regionalise = 1e5){
  
  # Check list has names
  if(is.null(names(rnl))){
    stop("rnet list has no names")
  }
  
  # Append rnet type to column names
  for(i in seq_along(rnl)){
    names(rnl[[i]]) <- ifelse(names(rnl[[i]]) %in% c("Gradient","Quietness","geometry"),
                              names(rnl[[i]]),
                              paste0(names(rnl)[i],"_",names(rnl[[i]]))
                              )
  }
  #lapply(rnl, head)
  # check for duplicated column names
  # only gradient and quietness should be in all rnets
  
  nms = sapply(rnl, names)
  nms = nms[!nms %in% c("Gradient","Quietness","geometry")]
  if(any(duplicated(nms))){
    stop("Duplicated column names in rnets")
  }
  
  # Make into a single data frame
  rnet_long = collapse::unlist2d(rnl, idcols = FALSE)
  rnet_long[] = lapply(rnet_long[], function(x){
    if(is.numeric(x)){
      x = tidyr::replace_na(x, 0)
    }
    x
  })
  rnet_long$geometry = sf::st_sfc(rnet_long$geometry, recompute_bbox = TRUE)
  rnet_long = sf::st_as_sf(rnet_long)
  
  names_overline = names(rnet_long)[names(rnet_long) != "geometry"]
  rnet_combined = stplanr::overline(rnet_long, 
                           attrib = names_overline,
                           fun = list(sum = sum, max = max),
                           regionalise = regionalise,
                           ncores = ncores)
  
  # Drop unneeded columns
  rnet_combined = rnet_combined[,!names(rnet_combined) %in% c("Gradient_sum","Quietness_sum")]
  
  # #saveRDS(rnet_combined, "outputdata/rnet_combined_after_overline.Rds")
  # # # Testing outputs
  # # rnet_combined = readRDS("outputdata/rnet_combined_after_overline.Rds")
  # rnet_combined = rnet_combined %>% 
  #   rowwise() %>% 
  #   mutate(Gradient = max(fastest_Gradient, balanced_Gradient, quietest_Gradient, ebike_Gradient)) %>% 
  #   mutate(Quietness = max(fastest_Quietness, balanced_Quietness, quietest_Quietness, ebike_Quietness)) 
  # 
  # rnet = rnet_combined %>% 
  #   select(-matches("_Q|_Gr")) %>% 
  #   mutate(across(matches("bicycle", round))) %>% 
  #   mutate(Gradient = round(Gradient, digits = 1))
  # # # TODO: check gradients
  # # table(rnet_combined$Gradient)
  # 
  # # see code/tests/test-quietness-network.R
  # rnet_bicycle = rnet %>% 
  #   select(matches("bicycle")) %>% 
  #   sf::st_drop_geometry()
  # rnet$total_cyclists_segment = rowSums(rnet_bicycle)
  # 
  # names(rnet)[1:8] = paste0("commute_", names(rnet))[1:8]
  # rnet = rnet %>% 
  #   filter(total_cyclists_segment > 0) %>% 
  #   select(-total_cyclists_segment) %>% 
  #   as.data.frame() %>% 
  #   sf::st_as_sf()
  # 
  
}

