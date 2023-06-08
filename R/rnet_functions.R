# Wrapper function for making rnets
# r routes
# type character e.g. "school_fastest"
# ncores passed to overline
# regionalise passed to overline

#TODO: inconsistent capitalisation Quietness vs quietness

make_rnets = function(r, ncores = 20, regionalise = 1e5){
  r = r[r$bicycle_go_dutch > 0 | r$bicycle > 0,]
  r$Gradient = round(r$gradient_smooth * 100)
  r$Quietness = round(r$quietness)
  
  rnet = stplanr::overline2(r, 
                            attrib = c("bicycle","bicycle_go_dutch","Quietness","Gradient"), 
                            fun = list(sum = sum, max = max),
                            ncores = ncores, regionalise = regionalise)
  
  rnet = rnet[,c("bicycle_sum","bicycle_go_dutch_sum","Gradient_max","Quietness_max")]
  names(rnet) = gsub("_sum$","",names(rnet))
  names(rnet) = gsub("_max$","",names(rnet))
  
  # Suppress low values, replace with 3
  rnet$bicycle <- round_sdc(rnet$bicycle)
  rnet$bicycle_go_dutch <- round_sdc(rnet$bicycle_go_dutch)
  
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

combine_rnets = function(rnl, ncores = 20, regionalise = 1e5, add_all = TRUE){
  
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
  rnet_combined = rnet_combined[,!grepl("_max$",names(rnet_combined)) | 
                                  names(rnet_combined) %in% c("Gradient_max","Quietness_max"),]
  # Rename columns
  names(rnet_combined) = gsub("_sum$","",names(rnet_combined))
  names(rnet_combined) = gsub("_max$","",names(rnet_combined))
  
  # Drop any roads with no cycling
  rnet_total = rnet_combined %>% 
      select(matches("bicycle")) %>%
      sf::st_drop_geometry()
  rnet_combined$total_cyclists_segment = rowSums(rnet_total)
  
  rnet_combined = rnet_combined[rnet_combined$total_cyclists_segment > 0,]
  rnet_combined$total_cyclists_segment = NULL
  
  # Calculate 'all' columns
  if(add_all){
    # TODO: Generalise this process for more trip purposes
    rnet_combined$all_fastest_bicycle = rnet_combined$school_fastest_bicycle + rnet_combined$commute_fastest_bicycle
    rnet_combined$all_fastest_bicycle_go_dutch = rnet_combined$school_fastest_bicycle_go_dutch + rnet_combined$commute_fastest_bicycle_go_dutch
    rnet_combined$all_quietest_bicycle = rnet_combined$school_quietest_bicycle + rnet_combined$commute_quietest_bicycle
    rnet_combined$all_quietest_bicycle_go_dutch = rnet_combined$school_quietest_bicycle_go_dutch + rnet_combined$commute_quietest_bicycle_go_dutch
    rnet_combined$all_balanced_bicycle = rnet_combined$school_balanced_bicycle + rnet_combined$commute_balanced_bicycle
    rnet_combined$all_balanced_bicycle_go_dutch = rnet_combined$school_balanced_bicycle_go_dutch + rnet_combined$commute_balanced_bicycle_go_dutch
    rnet_combined$all_ebike_bicycle = rnet_combined$school_ebike_bicycle + rnet_combined$school_ebike_bicycle
    rnet_combined$all_ebike_bicycle_go_dutch  = rnet_combined$school_ebike_bicycle_go_dutch + rnet_combined$commute_ebike_bicycle_go_dutch
  }

  return(rnet_combined)
}

#' Round and suppress low values

#' @examples
#' x = c(0, 1, 2.3, 9.9, 10, 10.1, 10.9, 20)
#' round_sdc(x)
round_sdc = function(x, threshold = 10, digits = 0, replacement = 3) {
  sel_sdc = x < threshold & x > 0
  x[sel_sdc] = replacement
  round(x, digits = digits)
}
