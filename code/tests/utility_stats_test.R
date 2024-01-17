tar_load(utility_stats)

names(utility_stats)
# leisure trips are missing because we have sliced the max flow trips only, and leisure trips have lower flows

test = utility_stats %>% 
  select(grep("all", names(utility_stats)))

dutch = utility_stats %>% 
  select(grep("dutch", names(utility_stats)))

ebike = utility_stats %>% 
  select(grep("ebike", names(utility_stats)))

base = utility_stats %>% 
  select(!grep("dutch", names(utility_stats)) & !grep("ebike", names(utility_stats)))

# Test consistency between scenarios - shopping
# these are not consistent

shopping_base = base %>% 
  select(grep("shopping", names(base)))

shopping_orig_base = shopping_base %>% 
  ungroup %>% 
  select(grep("orig", names(shopping_base)) &
           !grep("route", names(shopping_base)) &
           !grep("quiet", names(shopping_base)) &
           !grep("all", names(shopping_base)))
sum(shopping_orig_base, na.rm = T)
# [1] 9607.076

sum(utility_stats$shopping_orig_all, na.rm = T)
# [1] 9607.076

shopping_dutch = dutch %>% 
  select(grep("shopping", names(dutch)))

shopping_orig_dutch = shopping_dutch %>% 
  ungroup %>% 
  select(grep("orig", names(shopping_dutch)) &
           !grep("route", names(shopping_dutch)) &
           !grep("quiet", names(shopping_dutch)) &
           !grep("all", names(shopping_dutch)))
sum(shopping_orig_dutch, na.rm = T)
# [1] 17291.57


shopping_ebike = ebike %>% 
  select(grep("shopping", names(ebike)))

shopping_orig_ebike = shopping_ebike %>% 
  ungroup %>% 
  select(grep("orig", names(shopping_ebike)) &
           !grep("route", names(shopping_ebike)) &
           !grep("quiet", names(shopping_ebike)) &
           !grep("all", names(shopping_ebike)))
sum(shopping_orig_ebike, na.rm = T)
# [1] 14786.86

# Test shopping destinations

shopping_dest_base = shopping_base %>% 
  ungroup %>% 
  select(grep("dest", names(shopping_base)) &
           !grep("route", names(shopping_base)) &
           !grep("quiet", names(shopping_base)) &
           !grep("all", names(shopping_base)))
sum(shopping_dest_base, na.rm = T)
# [1] 9607.076

sum(utility_stats$shopping_dest_all, na.rm = T)
# [1] 9607.076

shopping_dest_dutch = shopping_dutch %>% 
  ungroup %>% 
  select(grep("dest", names(shopping_dutch)) &
           !grep("route", names(shopping_dutch)) &
           !grep("quiet", names(shopping_dutch)) &
           !grep("all", names(shopping_dutch)))
sum(shopping_dest_dutch, na.rm = T)
# [1] 17291.57

shopping_dest_ebike = shopping_ebike %>% 
  ungroup %>% 
  select(grep("dest", names(shopping_ebike)) &
           !grep("route", names(shopping_ebike)) &
           !grep("quiet", names(shopping_ebike)) &
           !grep("all", names(shopping_ebike)))
sum(shopping_dest_ebike, na.rm = T)
# [1] 14786.86



# Test consistency between scenarios - visiting
# these are consistent

visiting_base = base %>% 
  select(grep("visiting", names(base)))

visiting_orig_base = visiting_base %>% 
  ungroup %>% 
  select(grep("orig", names(visiting_base)) &
           !grep("route", names(visiting_base)) &
           !grep("quiet", names(visiting_base)) &
           !grep("all", names(visiting_base)))
sum(visiting_orig_base, na.rm = T)
# [1] 5405.999

sum(utility_stats$visiting_orig_all, na.rm = T)
# [1] 5405.999

visiting_dutch = dutch %>% 
  select(grep("visiting", names(dutch)))

visiting_orig_dutch = visiting_dutch %>% 
  ungroup %>% 
  select(grep("orig", names(visiting_dutch)) &
           !grep("route", names(visiting_dutch)) &
           !grep("quiet", names(visiting_dutch)) &
           !grep("all", names(visiting_dutch)))
sum(visiting_orig_dutch, na.rm = T)
# [1] 5405.999


visiting_ebike = ebike %>% 
  select(grep("visiting", names(ebike)))

visiting_orig_ebike = visiting_ebike %>% 
  ungroup %>% 
  select(grep("orig", names(visiting_ebike)) &
           !grep("route", names(visiting_ebike)) &
           !grep("quiet", names(visiting_ebike)) &
           !grep("all", names(visiting_ebike)))
sum(visiting_orig_ebike, na.rm = T)
# [1] 5405.999


# Test visiting destinations

visiting_dest_base = visiting_base %>% 
  ungroup %>% 
  select(grep("dest", names(visiting_base)) &
           !grep("route", names(visiting_base)) &
           !grep("quiet", names(visiting_base)) &
           !grep("all", names(visiting_base)))
sum(visiting_dest_base, na.rm = T)
# [1] 5405.999

sum(utility_stats$visiting_dest_all, na.rm = T)
# [1] 5405.999


visiting_dest_dutch = visiting_dutch %>% 
  ungroup %>% 
  select(grep("dest", names(visiting_dutch)) &
           !grep("route", names(visiting_dutch)) &
           !grep("quiet", names(visiting_dutch)) &
           !grep("all", names(visiting_dutch)))
sum(visiting_dest_dutch, na.rm = T)
# [1] 5405.999


visiting_dest_ebike = visiting_ebike %>% 
  ungroup %>% 
  select(grep("dest", names(visiting_ebike)) &
           !grep("route", names(visiting_ebike)) &
           !grep("quiet", names(visiting_ebike)) &
           !grep("all", names(visiting_ebike)))
sum(visiting_dest_ebike, na.rm = T)
# [1] 5405.999
