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
    for (i in modes_to_adjust) {
        # Only run for names that exist in the data:
        if (!i %in% names(routes)) next
        routes[[i]] = routes[[i]] * aadt_parameters$AADT_multiplier
    }
  routes
}