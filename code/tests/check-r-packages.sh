# Aim: check which R packages are installed from binaries to debug fail on large builds

# Find all packages installed with apt that contain the name "r-cran-"
# and print the package name and the version
dpkg -l | grep '^ii' | grep 'r-cran-' | awk '{print $2 " " $3}' 