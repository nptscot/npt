aadt_adjust = function(
    routes,
    aadt_parameters,
    purpose = "commute",
    modes_to_adjust = c("car", "other", "taxi", "foot", "bicycle", "public_transport", "all")
    # , bicycle_mode = "bicycle"
    ) {
  which_purpose = which(grepl(purpose, aadt_parameters$NPT_purpose, ignore.case = TRUE))
  aadt_parameters = aadt_parameters[which_purpose, ]
  # For testing:
  # tar_load(r_commute_fastest)
  
  if(purpose == "shopping") {
    routes = routes |> 
      mutate(
        bicycle_unchanged = bicycle,
        bicycle = bicycle * aadt_parameters$AADT_multiplier,
        mode_ratio_baseline = (all - bicycle) / (all - bicycle_unchanged),
        mode_ratio_baseline = case_when(is.infinite(mode_ratio_baseline) ~ 1, .default = mode_ratio_baseline),
        mode_ratio_baseline = case_when(is.nan(mode_ratio_baseline) ~ 0, .default = mode_ratio_baseline),
        car = car * mode_ratio_baseline,
        public_transport = public_transport * mode_ratio_baseline,
        foot = foot * mode_ratio_baseline,
        taxi = taxi * mode_ratio_baseline,
      ) |> 
      select(-bicycle_unchanged, -mode_ratio_baseline)
  } else {
    for (i in modes_to_adjust) {
      # Only run for names that exist in the data:
      if (!i %in% names(routes)) next
      routes[[i]] = routes[[i]] * aadt_parameters$AADT_multiplier
    }
  }
  routes
}
