# See https://github.com/MarcusRowcliffe/camtrapDensity for detailed instructions

# INITIAL SETUP
# One-off
install.packages(c("devtools", "dply", "tidyr", "jsonlite", "lubridate"))
devtools::install_github("inbo/camtraptor")
devtools::install_github("MarcusRowcliffe/camtrapDensity")

# LOAD PACKAGES
# At the beginning of each session
library(camtrapDensity)
library(camtraptor)
library(lubridate)

# LOAD DATA
# Assumes data are in the root project directory and Windows OS.
# Delete the "." for Mac.
pkg <- read_camtrapDP("./datapackage.json")

# CHECK DEPLOYMENT SCHEDULE
plot_deployment_schedule(pkg)

# SUBSET DEPLOYMENTS
# Example selects only deployments starting in 2023 or later and rechecks schedule
# Edit date as necessary
subpkg <- subset_deployments(pkg, start > ymd("2023-01-01"))
plot_deployment_schedule(subpkg)

# CHECK DEPLOYMENT CALIBRATION MODELS
pkg_chk <- check_deployment_models(pkg)

# REM ANALYSIS
res <- rem_estimate(pkg_chk, check_deployments=FALSE)

# EVALUATE MODEL FITS
plot(res$activity_model)
plot(res$radius_model, pdf=TRUE)
plot(res$angle_model)

# EXTRACT MODEL ESTIMATES
res$estimates
