
# Read data from the TEAMS folder
read_TEAMS = function(x){
  path_teams = Sys.getenv("NPT_TEAMS_PATH")
  if(nchar(path_teams) == 0){
    stop("Can't find Teams folder of secure data. Use usethis::edit_r_environ() to define NPT_TEAMS_PATH ")
  }
  
  if(file.exists(file.path(path_teams,x))){
    b_verylow = readRDS(file.path(path_teams, x))
  } else {
    stop("Can't find ",file.path(path_teams,x))
  }
}