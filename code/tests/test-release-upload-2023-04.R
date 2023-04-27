# Aim: test the release of build process, does it include zip and pmtiles

# Set-up, based on small build:
tar_load(combined_network)
tar_load(save_outputs)
tar_source()

# This is the contents of the target:
length(combined_network)
length(r_commute)
commit = gert::git_log(max = 1)
message("Commit: ", commit)

if(Sys.info()[['sysname']] == "Linux") {
  v = paste0("v", save_outputs, "_commit_", commit$commit)
  v = gsub(pattern = " |:", replacement = "-", x = v)
  setwd("outputdata")
  f = list.files(path = ".", pattern = "Rds|zip|pmtiles")
  # Piggyback fails with error message so commented and using cust
  # piggyback::pb_upload(f)
  msg = glue::glue("gh release create {v} --generate-notes")
  message("Creating new release and folder to save the files: ", v)
  dir.create(v)
  message("Going to try to upload the following files: ", paste0(f, collapse = ", "))
  message("With sizes: ", paste0(fs::file_size(f), collapse = ", "))
  system(msg)
  for(i in f) {
    gh_release_upload(file = i, tag = v)
    # Move into a new directory
    file.copy(from = i, to = file.path(v, i))
  }
  message("Files stored in output folder: ", v)
  message("Which contains: ", paste0(list.files(v), collapse = ", "))
  # For specific version:
  # system("gh release create v0.0.1 --generate-notes")
  file.remove(f)
  setwd("..")
}  else {
  message("gh command line tool not available")
  message("Now create a release with this version number and upload the files")
}

