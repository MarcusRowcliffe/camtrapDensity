# See https://github.com/MarcusRowcliffe/camtrapDensity for detailed instructions

# 1. INITIAL SETUP
# One-off
install.packages(c("devtools", "dply", "tidyr", "jsonlite", "lubridate"))
devtools::install_github("inbo/camtraptor")
devtools::install_github("MarcusRowcliffe/camtrapDensity")

# 2. LOAD PACKAGES
# At the beginning of each session
library(camtrapDensity)
library(camtraptor)
library(lubridate)

# 3. LOAD DATA
# Assumes data are in the root project directory and Windows OS.
# Delete the "." for Mac.
pkg <- read_camtrapDP("./DataPackage_V1.0/datapackage.json")
plot_deployment_schedule(pkg)

# 4. FILTER DATA (IF NECESSARY)
# e.g. selects only deployments occurring within 2023
subpkg <- subset_deployments(pkg, choice=end < ymd("2024-10-20"))
# slices all deployments to given date
subpkg <- slice_camtrap_dp(pkg,
                           start = "2017/10/09",
                           end = "2017/10/26")
# slices deployment at location S02 to end at the given date
subpkg <- slice_camtrap_dp(pkg,
                           end = "2017/10/27",
                           depChoice = locationName=="S02")
plot_deployment_schedule(subpkg)

# 5. CHECK DEPLOYMENT SCHEDULE
plot_deployment_schedule(pkg)
plot_deployment_schedule(subpkg)

# 6. CHECK DEPLOYMENT CALIBRATION MODELS
pkg_chk <- check_deployment_models(subpkg)

# 7. REM ANALYSIS
res <- rem_estimate(pkg_chk, check_deployments=FALSE)

# 8. EVALUATE MODEL FITS
plot(res$activity_model)
plot(res$radius_model, pdf=TRUE)
plot(res$angle_model)

# 9. EXTRACT MODEL ESTIMATES
res$estimates

# 10. SAVE WORKSPACE FOR FUTURE REFERENCE
save.image()
