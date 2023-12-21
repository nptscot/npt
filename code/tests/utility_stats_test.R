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

shopping_base = base %>% 
  select(grep("shopping", names(base)))

shopping_orig_base = shopping_base %>% 
  select(grep("orig", names(shopping_base)) &
           !grep("route", names(shopping_base)) &
           !grep("quiet", names(shopping_base)))
summary(shopping_orig_base)
sum(shopping_orig_base$shopping_orig_all, na.rm = T)
sum(base$shopping_orig_all, na.rm = T)
