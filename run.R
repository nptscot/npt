#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

setwd("~/github/nptscot/npt")
targets::tar_make()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint

# Set up cron job to run weekley on Sunday at 1AM:
# crontab -e
# 0 1 * * 0 /usr/lib/R/bin/Rscript '/home/atumscott/nptscot/npt/run.R'  >> '/home/atumscott/nptscot/npt/run.log' 2>&1






# # Run with cron via R (legacy):
# targets::tar_visnetwork(targets_only = TRUE)
# install.packages("cronR", dependencies = TRUE)
# targets::tar_destroy()
# cronR::cron_rstudioaddin()
# remotes::install_cran("cronR")
# library(cronR)
# f = "/home/atumscott/nptscot/npt/run.R"
# cmd <- cron_rscript(f)
# cmd

# 
# # cron_add(command = cmd, frequency = 'minutely', id = 'test1', description = 'My process 1', tags = c('lab', 'xyz'))
# Set cron job to run weekly on Sunday at 1AM:
# cron_add(command = cmd, frequency = 'weekly', at='1AM', id = 'rebuild_atum')
# y
# Daily (a bit intense!):
# cron_add(command = cmd, frequency = 'daily', at='1AM', id = 'rebuild_atum')

# cron job to run daily:
## cronR job
## id:   rebuild_atum
## tags: 
## desc: 
# Try to run daily?
# 26 15 * * * /usr/lib/R/bin/Rscript '/home/atumscott/nptscot/npt/run.R'  >> '/home/atumscott/nptscot/npt/run.log' 2>&1

# cron job to run weekly:
## cronR job
## id:   rebuild_atum
## tags:
## desc:
# 0 1 * * 0 /usr/lib/R/bin/Rscript '/home/atumscott/nptscot/npt/run.R'  >> '/home/atumscott/nptscot/npt/run.log' 2>&1