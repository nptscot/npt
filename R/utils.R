gh_release_upload = function(file, tag = "v1") {
  msg = glue::glue("gh release upload {tag} {file} --clobber")
  message("Running this command:\n", msg)
  system(msg)
}
gh_release_downlad = function(file, tag = "v1") {
  msg = glue::glue("gh release download {tag} {file}")
  message("Running this command:\n", msg)
  # system(msg)
  system("gh release download -p '*.*'")
}