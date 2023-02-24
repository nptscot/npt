gh_release_upload = function(file, tag = "v1") {
  msg = glue::glue("gh release upload {tag} {file} --clobber")
  message("Running this command:\n", msg)
  system(msg)
}
gh_release_downlad = function(file = "'*.*'", tag = "v1") {
  msg = glue::glue("gh release download {tag} {file}")
  message("Running this command:\n", msg)
  # system(msg)
  system(glue::glue("gh release download -p {file}"))
}

gh_installed = function() {
  # if(Sys.info()[['sysname']] == "Windows") {
  #   sysoutput = system("powershell", intern = TRUE)
  #   if(sysoutput[1] == "Windows PowerShell") {
  #     return("PowerShell")
  #   }
  # }
  sysoutput = processx::run("gh", error_on_status = FALSE)
  if(sysoutput$stdout == 0) message("gh exists")
}