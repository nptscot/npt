#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

setwd("~/nptscot/npt/")
targets::tar_make()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint

# # Run with cron
# targets::tar_visnetwork(targets_only = TRUE)
# install.packages("cronR", dependencies = TRUE)
# targets::tar_destroy()
# cronR::cron_rstudioaddin()
# library(cronR)
# f = "/home/atumscott/nptscot/npt/run.R"
# cmd <- cron_rscript(f)
# cmd
# 
# # # cron_add(command = cmd, frequency = 'minutely', id = 'test1', description = 'My process 1', tags = c('lab', 'xyz'))
# cron_add(command = cmd, frequency = 'daily', at='1AM', id = 'rebuild_atum')