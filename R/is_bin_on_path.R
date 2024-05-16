is_bin_on_path = function(bin) {
  exit_code = suppressWarnings(system2("command", args = c("-v", bin), stdout = FALSE))
  return(exit_code == 0)
}

find_odjitter_location = function() {
  # Find odjitter location
  odjitter_location = NULL
  if (!is_bin_on_path("odjitter")) {
    if (Sys.getenv("ODJITTER_PATH") != "") {
      odjitter_location = Sys.getenv("ODJITTER_PATH")
      return(odjitter_location)
    }
    old_path = Sys.getenv("PATH")
    Sys.setenv(PATH = paste(old_path, "/root/.cargo/bin", sep = ":"))
    odjitter_location = "/root/.cargo/bin/odjitter"
  } else {
    odjitter_location = "odjitter"
  }
  odjitter_location
}
