#!/bin/bash

# Submit the pipeline as a background process with ./run.sh
# module load R # Uncomment if R is an environment module.
nohup nice -4 R CMD BATCH code/build.R &

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
# rm -f .RData

# # Get data if it doesn't exist:
# fi [ ! -d inputdata ]; then
# gh repo clone nptscot/inputdata
# fi
