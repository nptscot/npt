aadt_adjust = function(routes, aadt_parameters, purpose = "commute") {
    which_purpose = which(grepl(purpose, aadt_parameters$NPT_purpose, ignore.case = TRUE))
    aadt_parameters = aadt_parameters[which_purpose, ]

}