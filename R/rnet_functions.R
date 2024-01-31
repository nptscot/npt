# Wrapper function for making rnets
# r routes
# type character e.g. "school_fastest"
# ncores passed to overline
# regionalise passed to overline

#TODO: inconsistent capitalisation Quietness vs quietness

make_rnets = function(r, ncores = 1, regionalise = 1e5){
  r = r[r$bicycle_go_dutch > 0 | r$bicycle > 0 | r$bicycle_ebike > 0,]
  #r$gradient = round(r$gradient_smooth * 100)
  #r$quietness = round(r$quietness)
  
  rnet = stplanr::overline2(r, 
                            #attrib = c("bicycle","bicycle_go_dutch","bicycle_ebike","quietness","gradient"),
                            attrib = c("bicycle","bicycle_go_dutch","bicycle_ebike"),
                            #fun = list(sum = sum, max = first),
                            fun = sum,
                            ncores = ncores,
                            regionalise = regionalise)
  
  #rnet = rnet[,c("bicycle_sum","bicycle_go_dutch_sum","bicycle_ebike_sum","gradient_max","quietness_max")]
  #names(rnet) = gsub("_sum$","",names(rnet))
  #names(rnet) = gsub("_max$","",names(rnet))
  
  # Suppress low values, replace with 3
  rnet$bicycle = round_sdc(rnet$bicycle)
  rnet$bicycle_go_dutch = round_sdc(rnet$bicycle_go_dutch)
  rnet$bicycle_ebike = round_sdc(rnet$bicycle_ebike)
  
  rnet
}

# Function to merge list of rnets into a single rnet
# rnl list of rnets


# Takes a list of rnets and combines into a single rnet
# Names of the list are appended to the start of column names
# e.g. "commute_fastest_" is addeded to "bicycle_go_dutch"
# gradient & quietness are not renamed

combine_rnets = function(rnl, ncores = 1, regionalise = 1e5, add_all = TRUE){
  
  # Check list has names
  if(is.null(names(rnl))){
    stop("rnet list has no names")
  }
  
  # Append rnet type to column names
  for(i in seq_along(rnl)){
    names(rnl[[i]]) = ifelse(names(rnl[[i]]) %in% c("gradient","quietness","geometry"),
                              names(rnl[[i]]),
                              paste0(names(rnl)[i],"_",names(rnl[[i]]))
                              )
  }
  #lapply(rnl, head)
  # check for duplicated column names
  # only gradient and quietness should be in all rnets
  
  nms = sapply(rnl, names)
  nms = nms[!nms %in% c("gradient","quietness","geometry")]
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
  rnet_combined = stplanr::overline(sl = rnet_long, 
                           attrib = names_overline,
                           fun = list(sum = sum, max = max),
                           regionalise = regionalise,
                           ncores = ncores)
  
  # Drop unneeded columns
  rnet_combined = rnet_combined[,!names(rnet_combined) %in% c("gradient_sum","quietness_sum")]
  rnet_combined = rnet_combined[,!grepl("_max$",names(rnet_combined)) | 
                                  names(rnet_combined) %in% c("gradient_max","quietness_max"),]
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
    # Structure is purpose_route_variable
    purps = strsplit(names(rnet_combined),"_")
    purps = unique(sapply(purps, function(x){x[1]}))
    purps = purps[!purps %in% c("gradient","quietness","geometry")]
    purps = paste0(purps,"_")
    
    vars = names(rnet_combined)
    vars = vars[!vars %in% c("gradient","quietness","geometry")]
    for(i in seq_along(purps)){
      vars = gsub(purps[i],"",vars)
    }
    vars = unique(vars)
    
    for(i in seq_along(vars)){
      rnet_combined[paste0("all_",vars[i])] = rowSums(sf::st_drop_geometry(rnet_combined[,grepl(paste0(vars[i],"$"),names(rnet_combined))]))
    }
    
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

# Return the first of a vector
first = function(x){
  x[1]
}

#' Convert segments into an rnet
segments2rnet = function(segments){
  segments$gradient = round(segments$gradient_smooth * 100)
  segments$quietness = round(segments$quietness)
  segments = stplanr::overline2(segments, c("quietness","gradient"), fun = first)
  segments
}

